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
	"dymium.com/server/protocol"
	_ "github.com/lib/pq"
)

/*
	"dymium.com/server/gotypes"
	"dymium.com/server/protocol"
	"github.com/dgrijalva/jwt-go"
*/

var psqlInfo string
var db *sql.DB

type Virtcon struct {
	sock   net.Conn
	tenant string
	target string
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
	addr, err := net.ResolveTCPAddr("tcp", fmt.Sprintf("127.0.0.1:%d", port))
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
	conmap map[int]*Virtcon, counter chan int, mu *sync.RWMutex) {

	listener.SetDeadline(time.Now().Add(30*time.Second))

	for {
		ingress, err := listener.Accept() //*TCPConn
		if err, ok := err.(*net.OpError); ok && err.Timeout() {
				log.Infof("timeout")
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

	for i := 0; i < len(connections); i++ {
		tg := strings.Split(connections[i], ",")
		listen, err := createListener(customer, tg[0], tg[1])
		if err != nil {
			log.ErrorTenantf(customer, "Error creating listener %s", err.Error())
		}
		listeners = append(listeners, listen)
		go listenLocally(egress, listen, customer, tg[0], messages, conmap, counter, &mu)
	}
	// read and write from and to connector over tls
	go remotepipe(customer, messages, enc, dec, egress, conmap, counter, &mu, listeners)
}

func Server(address string, port int, customer string,
	certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte,
	dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls string) {
	var err error

	initDB(dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls)
	//go logBandwidth(customer)

	cer, err := tls.X509KeyPair(certPEMBlock, keyPEMBlock)
	if err != nil {
		log.Errorf("Error decoding KeyPair", err.Error())
		os.Exit(1)
	}

	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM(caCert)

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
			log.Errorf("client: error during handshake, error: %s", err.Error())
			continue
		}

		var connections []string
		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
		for _, cert := range state.PeerCertificates {
			for _, name := range cert.DNSNames {
				log.Debugf("Connection: %s", name)
				connections = append(connections, name)
			}
		}
		log.Debugf("go to requestConnections")
		go requestConnections(egress, customer, connections)
	}
}
