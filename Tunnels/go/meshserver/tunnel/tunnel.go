package tunnel

import (
	"crypto/tls"
	"crypto/x509"
	"database/sql"
	"encoding/gob"
	"fmt"
	"net"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
	"dymium.com/dymium/log"
	"golang.org/x/net/context"
	"dymium.com/server/protocol"
	"dymium.com/meshserver/types"	
	_ "github.com/lib/pq"
)

var psqlInfo string
var db *sql.DB

type Virtcon struct {
	sock   net.Conn
	tenant string
	target string
}

type TunnelPipe struct {
	sock      net.Conn 
	connid    string 
	targets   []string
	tunnelid  []string
}

var pipes (map[string]*TunnelPipe)
var mupipes sync.RWMutex

type TunnelUpdate struct {
	 Cid string
	 Tid string 
}

func initDB(host, nport, user, password, dbname, usetls string) error {
	psqlInfo = fmt.Sprintf("host=%s port=%s user=%s "+
		"dbname=%s sslmode=%s",
		host, nport, user, dbname, usetls)
	if password != "" {
		psqlInfo += " password='" + password + "'"
	}
	var err error
	log.Debugf("%s", psqlInfo)
	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Errorf("Error creating database: %s", err.Error())
	}
	return err
}

func createListener(customer, target, sport string) (*net.TCPListener, error) {
	port, err := strconv.Atoi(sport)
	if err != nil {
		log.ErrorTenantf(customer, "Error parsing port: %s, %s", sport, err.Error())
		return nil, err
	}
	sink := fmt.Sprintf(":%d", port)
	log.Infof("Create listener on %s", sink)
	addr, err := net.ResolveTCPAddr("tcp", sink)
	if err != nil {
		log.Errorf("Error resolving address: %s", err.Error())
		panic(err)
	}
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		log.Errorf("Error in ListenTCP(%s): %s, can't continue", addr, err.Error())
		os.Exit(1)
	}
	return listener, err
}

func remotepipe(customer string, messages chan protocol.TransmissionUnit, enc *gob.Encoder, dec *gob.Decoder,
	egress net.Conn, conmap map[int]*Virtcon, counter chan int, mu *sync.RWMutex,
	listeners []*net.TCPListener) {

	// writer
	go func(messages chan protocol.TransmissionUnit, enc *gob.Encoder) {
		for {
			tosend := <-messages
			err := enc.Encode(tosend)
			if err != nil {
				log.ErrorTenantf(customer, "Error in Encode: %s", err.Error())
				return
			} else {
				log.Debugf("sent %d bytes to connector", len(tosend.Data))
			}
		}

	}(messages, enc)
	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)

		if err != nil {
			log.ErrorTenantf(customer, "Read from client failed '%s', cleanup the proxy connection!", err.Error())
			// close all outgoing connections
			mu.Lock()
			for key := range conmap {
				conmap[key].sock.Close()
				delete(conmap, key)
			}
			mu.Unlock()
			egress.Close()
			for i:= 0; i < len(listeners); i++ {
				listeners[i].Close()
			}
			return
		}

		switch buff.Action {
		case protocol.Open:
			log.Debugf("protocol.Open. Should not happen")
		case protocol.Close:
			log.Debugf("protocol.Close for id=%d", buff.Id)
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if  ok {
				log.InfoTenantf(customer, "Connection #%d closing, %d left", buff.Id, len(conmap)-1)
				conn.sock.Close()
				mu.Lock()
				delete(conmap, buff.Id)
				mu.Unlock()
			} else {
				log.Errorf("Error finding the descriptor %d, %v", buff.Id, buff)
			}
		case protocol.Send:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if  ok {
				_, err = conn.sock.Write(buff.Data)

				if err != nil {
					log.ErrorTenantf(customer, "Write to local socket(%d) error: %s", buff.Id, err.Error())
					conn.sock.Close()
				} else {
					log.DebugTenantf(customer, "Wrote to local socket(%d) bytes: %d", buff.Id, len(buff.Data))
				}
			} else {
				log.ErrorTenantf(customer, "Error finding the descriptor %d, %v", buff.Id, buff)
			}
		}
	}
}

func localpipe(ingress net.Conn, egress net.Conn, messages chan protocol.TransmissionUnit,
	conmap map[int]*Virtcon, id int, connectionString string, mu *sync.RWMutex) {
	
	// open connection on the other side
	log.Infof("send open to connector for %s", connectionString)
	out := protocol.TransmissionUnit{protocol.Open, id, []byte(connectionString) }
	messages <- out 

	for {
		buff := make([]byte, 0xffff)
		n, err := ingress.Read(buff)
		mu.RLock()
		conn, ok := conmap[id]
		mu.RUnlock()
		if err != nil {
			if strings.Contains(err.Error(), "use of closed network connection") {
				// no op
			} else {
				if ok {
					log.ErrorTenantf(conn.tenant,
						"Local read failed '%s', id:%d", err.Error(), id)
				} else {
					log.Errorf("Local read failed '%s', id:%d", err.Error(), id)
				}
				ingress.Close()
			}
			//egress.Close()
			out := protocol.TransmissionUnit{protocol.Close, id, nil}
			messages <- out
			return
		}
		if n == 0 {
			log.Infof("Read returned 0 bytes")
		}
		log.Debugf("read local data successfully %d bytes", n)
		b := buff[:n]
		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
		messages <- out
	}
}

func listenLocally(egress net.Conn, listener *net.TCPListener, customer string,
	connectionString string, messages chan protocol.TransmissionUnit,
	conmap map[int]*Virtcon, counter chan int, mu *sync.RWMutex, localport string) {

	listener.SetDeadline(time.Now().Add(30*time.Second))
	log.Infof("Listener waits for local connection for %s on :%s", connectionString, localport)
	for {
		ingress, err := listener.Accept() //*TCPConn
		if err, ok := err.(*net.OpError); ok && err.Timeout() {
				listener.SetDeadline(time.Now().Add(30*time.Second))
				continue
		} else {
			if err != nil {
				log.Errorf("Error in Accept: %s", err.Error())
				return
			}
		}
		
		log.Infof("accepted local connection for %s", connectionString)
		v := Virtcon{}
		v.sock = ingress
		v.target = connectionString
		v.tenant = customer

		connectionCounter := <-counter
		mu.Lock()
		conmap[connectionCounter] = &v
		mu.Unlock()
		go localpipe(ingress, egress, messages, conmap, connectionCounter, connectionString, mu)

	}
}
func requestConnections(egress net.Conn, customer string, connections []string) {
	// per connector
	var conmap = make(map[int]*Virtcon)
	var mu sync.RWMutex

	messages := make(chan protocol.TransmissionUnit, 50)

	dec := gob.NewDecoder(egress)
	enc := gob.NewEncoder(egress)

	counter := make(chan int, 1)
	go func(counter chan int) {
		i := 0
		for {
			counter <- i
			i += 1
		}
	}(counter)


	var listeners []*net.TCPListener
	var pipe TunnelPipe 
	pipe.sock = egress 

	for i := 0; i < len(connections); i++ {
		tg := strings.Split(connections[i], ",")
		pipe.connid = tg[2]

		listen, err := createListener(customer, tg[0], tg[1])
		if err != nil {
			log.ErrorTenantf(customer, "Error creating listener %s", err.Error())
		} else {
			log.Infof("Created listener for %s %s, connector %s, tunnel %s", tg[0], tg[1], tg[2], tg[3])
		}
		pipe.tunnelid = append(pipe.tunnelid, tg[3])
		pipe.targets = append(pipe.targets, tg[0])
		listeners = append(listeners, listen)

		mupipes.Lock()
		pipes[tg[2]] = &pipe
		mupipes.Unlock()

		go listenLocally(egress, listen, customer, tg[0], messages, conmap, counter, &mu, tg[1])
	}
	
	// read and write from and to connector over tls
	go remotepipe(customer, messages, enc, dec, egress, conmap, counter, &mu, listeners)

	
}


func GetConnectors(schema string) ( []types.Connector, error) {
	var ctx context.Context
	var out []types.Connector //
	var pswd = "***********"

    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, err := db.BeginTx(ctx, nil)
	sql := `select a.id, a.name, a.accesskey, a.accesssecret, EXTRACT(epoch from (now() - a.createdat)), COALESCE(b.use_connector, false) from `+schema+`.connectorauth as a left join `+schema+`.connections as b on a.id=b.connector_id`

	rows, err  := tx.QueryContext(ctx, sql)
	if nil == err {
		defer rows.Close()
		for rows.Next() {
			var id, name, accesskey, accesssecret string 
			var status bool
			var age float64

			err = rows.Scan(&id, &name, &accesskey, &accesssecret, &age, &status)
			if err != nil {
				tx.Rollback()
				log.Errorf("GetConnectors error 0: %s", err.Error())
				return out, err			
			}
			var o  types.Connector 
			o.Name = name 
			o.Id = id 
			o.Accesskey = &accesskey 
			if age > 60*60*24 {
				o.Secret = &pswd
			} else {
				o.Secret = &accesssecret
			}
			var st string
			if status {
				st = "provisioned"
			} else {
				st = "configured"
			}
			t := types.TunnelStatus(st)
			o.Status = &t
			out = append(out, o)
		}

		for i:=0; i < len(out); i++ {
			sql := `select id, targetaddress, targetport, localport, connectorname, status from ` +schema+ 
			`.connectors where id_connectorauth=$1;`

			trows, err  := tx.QueryContext(ctx, sql, out[i].Id)
			if err != nil {
				tx.Rollback()
				log.Errorf("GetConnectors error 1: %s", err.Error())
				return out, err			
			}
			defer trows.Close()
			out[i].Tunnels = []types.Tunnel{}
			for trows.Next() {
				var id, targetaddress, targetport, localport, connectorname string 
				var status types.ConnectorStatus

				err = trows.Scan(&id, &targetaddress, &targetport, &localport, &connectorname, &status)
				if err != nil {
					tx.Rollback()
					log.Errorf("GetConnectors error 2: %s", err.Error())
					return out, err			
				}
				t := types.Tunnel{ &id, connectorname, targetaddress, targetport, &localport, &status }		
				out[i].Tunnels = append(out[i].Tunnels, t)		
			}			
		}
	} else {
		tx.Rollback()
		log.Errorf("GetConnectors error 0: %s", err.Error())
		return out, err
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewConnector error 4: %s", err.Error())
		return out, err
	}	

	return out, nil
}
func overseer(customer string) {
	
	for {
		time.Sleep(60 * time.Second)
		// pull down the current configuration 
		conns, err := GetConnectors(customer)
		if err != nil {
			log.Errorf("Error getting connector info: %s", err.Error())
			continue
		}

		mupipes.Lock()
		for id, c := range pipes {
			found := false
			ifound := 0
			for i := 0; i < len(conns); i++ {
				if conns[i].Id == id {
					found = true
					ifound = i
					break
				}
			}
			if !found {
				c.sock.Close()
				log.Infof("Connector %s deleted, disconnecting...", id)
				delete(pipes, id)
				continue
			}
			// figure out if connector has changed
			if( len(c.tunnelid) != len(conns[ifound].Tunnels) ) {
				log.Infof("Connector %s changed: %d tunnels to %d tunnels\npreparing to reset, disconnecting...", 
					id, len(c.tunnelid), len(conns[ifound].Tunnels))
				c.sock.Close()
				delete(pipes, id)
				continue				
			}

			// ok, same length, then we can iterate over one, and should find a counterpart
			for i:=0; i < len(c.tunnelid); i++ {
				tid := c.tunnelid[i]
				target := c.targets[i]
				jfound := -1
				for j := 0; j < len(conns[ifound].Tunnels); j++ {
					if *conns[ifound].Tunnels[j].Id == tid {
						jfound = j
						s := fmt.Sprintf("%s:%s", conns[ifound].Tunnels[j].Address, conns[ifound].Tunnels[j].Port)
						if target != s {
							log.Infof("Connector %s changed: tunnel %s has target changed\npreparing to reset, disconnecting...", 
							id, tid)
							c.sock.Close()
							delete(pipes, id)
							i = len(c.tunnelid)
							break			
						}
					}
				}
				if jfound == -1 {
					log.Infof("Connector %s changed: tunnel %s not found\npreparing to reset, disconnecting...", 
					id, tid)
					c.sock.Close()
					delete(pipes, id)
					continue							
				}

			}

		}
		var tunnels []TunnelUpdate
		/*
		type TunnelUpdate struct {
			Cid string
			Tid string 
	   } */
		for id, c := range pipes {
			for i:=0; i < len(c.tunnelid); i++ {
				a := TunnelUpdate{id, c.tunnelid[i]}
				tunnels = append(tunnels, a)
			}
		}
		mupipes.Unlock()

		// send update to mark tunnels as used
		UpdateTunnels(customer, tunnels)
		
	}
}

func UpdateTunnels(customer string, tunnels []TunnelUpdate) {
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, _ := db.BeginTx(ctx, nil)

	sql := `UPDATE ` + customer + `.connectors set status='provisioned'`
	tx.ExecContext(ctx, sql)

	for i := 0; i < len(tunnels); i++ {
		sql = `UPDATE ` + customer + `.connectors set status='active' where id=$1 and id_connectorauth=$2`
		_, err := tx.ExecContext(ctx, sql, tunnels[i].Tid, tunnels[i].Cid )
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
		}
	}

	err := tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateTunnels error 4: %s", err.Error())
		return
	}	
}

func Server(address string, port int, customer string,
	certPEMBlock, keyPEMBlock []byte, caCert []byte,
	dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls string) {
	var err error

	initDB(dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls)
	//go logBandwidth(customer)

	pipes = make(map[string]*TunnelPipe)

	cer, err := tls.X509KeyPair(certPEMBlock, keyPEMBlock)
	if err != nil {
		log.Errorf("Error decoding KeyPair", err.Error())
		os.Exit(1)
	}

	log.Infof("CA cert: %s", caCert)
	caCertPool := x509.NewCertPool()
	ok := caCertPool.AppendCertsFromPEM(caCert)
	log.Infof("AppendCertsFromPEM returned %t", ok)

	config := &tls.Config{
		Certificates: []tls.Certificate{cer},
		ClientCAs:    caCertPool,
		ClientAuth:   tls.RequireAndVerifyClientCert,
	}
	connect := fmt.Sprintf("%s:%d", address, port)
	log.Debugf("TLS listen on %s", connect)
	ln, err := tls.Listen("tcp", connect, config)

	if err != nil {
		log.Errorf("Error in tls.Listen: %s", err.Error())
		os.Exit(1)
	}
	defer ln.Close()
	go overseer(customer)
	for {
		// call from a connector
		egress, err := ln.Accept()
		if err != nil {
			log.Errorf("Error in tls.Accept: %s", err.Error())
			continue
		}
		log.Debugf("Accepted!")
		// get the underlying tls connection
		tlsConn, ok := egress.(*tls.Conn)
		if !ok {
			log.Errorf("server: erm, this is not a tls conn")
			continue
		}
		// perform handshake
		if err := tlsConn.Handshake(); err != nil {
			log.Errorf("Error during handshake: %s", err.Error())
			continue
		}

		var connections []string
		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
		for _, cert := range state.PeerCertificates {
			log.Debugf("Subject %s", cert.Subject)
			for _, name := range cert.DNSNames {
				log.Debugf("Remote Connector requested: %s", name)
				connections = append(connections, name)
			}
		}
		log.Debugf("go to requestConnections")
		go requestConnections(egress, customer, connections)
	}
}
