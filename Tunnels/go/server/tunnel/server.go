package tunnel

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/gob"
	"fmt"
	"log"
	"net"
	"os"
	"strings"
	"time"
	"database/sql"
	_ "github.com/lib/pq"
	"dymium.com/server/protocol"
)

var iprecords []net.IP
var ipindex = 0
var bytesInLog = make(chan int, 128)
var bytesOutLog = make(chan int, 128)

func logBandwidth(customer string) {
	bytesIn := 0
	bytesOut := 0
	for  {
		select {
		case _bytesIn := <-bytesInLog:
			bytesIn += _bytesIn
			log.Printf("bytesIn=%d\n", bytesIn)
		case _bytesOut := <-bytesOutLog:
			bytesOut += _bytesOut
			log.Printf("bytesOut=%d\n", bytesOut)
		case <-time.After(30 * time.Second):
			if(	bytesIn != 0 || bytesOut != 0) {
				sql := `update ` + customer + `.counters set bytesin=bytesin+$1, bytesout=bytesout+$2 where id=1;`
				log.Printf("sql: %s\n", sql)
				_, err := db.Exec(sql, bytesIn, bytesOut)
				if err != nil {
					log.Printf("Error saving bytes: %s\n", err.Error() )
				}
			}
			bytesIn = 0
			bytesOut = 0			
		}	
	}
}

var psqlInfo string
var db *sql.DB
func initDB(host, nport, user, password, dbname, usetls string) error {
	psqlInfo = fmt.Sprintf("host=%s port=%s user=%s "+
	"dbname=%s sslmode=%s",
	host, nport, user, dbname, usetls)
	if password != "" {
	psqlInfo += " password='"+password+"'"
	}
	var err error
	log.Printf("%s\n", psqlInfo)
	db, err = sql.Open("postgres", psqlInfo)
	if(err != nil) {
		log.Printf("Error creating database: %s\n", err.Error())
	}
	return err
}
func displayBuff(what string, buff []byte) {
	if len(buff) > 14 {
		head := buff[:6]
		tail := buff[len(buff) - 6:]
		log.Printf("%s head: %v, tail: %v\n", what, head, tail)
	} else {
		log.Printf("%s buffer: %v\n", what, buff)
	}

}
func getTargetConnection(customer, postgresPort string) (net.Conn, error) {
	//log.Printf("in getTargetConnection, len(iprecords): %d\n", len(iprecords))
	index := ipindex % len(iprecords)
	ipindex = (ipindex + 1) % len(iprecords)

	target := iprecords[index].String() + ":" + postgresPort
	log.Printf("target: %s\n", target)
	return net.Dial("tcp", target)
}

func Server(address string, port int, customer, postgressDomain, postgresPort string, 
	certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte, 
	dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls string) {
	var err error

	initDB(dbDomain, dbPort, dbUsername, dbPassword, dbName, usetls) 
	go logBandwidth(customer)
	targetHost := customer + postgressDomain
	iprecords, err = net.LookupIP(targetHost)
	log.Printf("target postgress: %s\n", targetHost)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	for _, ip := range iprecords {
		log.Printf("db endpoint: %s\n", ip)
	}

	cer, err := tls.X509KeyPair(certPEMBlock, keyPEMBlock)
	if err != nil {
		log.Println(err)
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
	log.Printf("TLS listen on %s\n", connect)
	ln, err := tls.Listen("tcp", connect, config)

	if err != nil {
		log.Printf("Error in tls.Listen: %s\n", err.Error())
		os.Exit(1)
	}
	defer ln.Close()

	for {
		ingress, err := ln.Accept()

		if err != nil {
			log.Printf("Error in tls.Accept: %s\n", err.Error())
			continue
		}
		// get the underlying tls connection
		tlsConn, ok := ingress.(*tls.Conn)
		if !ok {
			log.Println("server: erm, this is not a tls conn")
			continue
		}
		// perform handshake
		if err := tlsConn.Handshake(); err != nil {
			log.Printf("client: error during handshake, error: %s\n", err.Error())
			continue
		}

		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
		for _, cert := range state.PeerCertificates {
			for _, name := range cert.DNSNames {
				log.Printf("Group: %s\n", name)
			}
		}

		go proxyConnection(ingress, customer, postgresPort)
	}
}

func pipe(egress net.Conn, messages chan protocol.TransmissionUnit, id int) {

	for {
		buff := make([]byte, 0xffff)		
		n, err := egress.Read(buff)
		if err != nil {
			if(strings.Contains(err.Error(), "use of closed network connection" ) ){
				// no op
			} else {
				log.Printf("Db read failed '%s', id:%d\n", err.Error(), id)
			}
			egress.Close()
			out := protocol.TransmissionUnit{protocol.Close, id, nil}
			messages <- out
			return
		}
		b := buff[:n]
		bytesOutLog <- n
		//displayBuff("Read from db ", b)
		//log.Printf("Send to client %d bytes, connection %d\n", n, id)
		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
		messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, enc *gob.Encoder, ingress net.Conn, conmap map[int]net.Conn) {
	for {
		buff, ok := <-messages
		if !ok {
			close(messages)
			return
		}

		err := enc.Encode(buff)
		if err != nil {
			log.Printf("Error in encoder: %s\n", err.Error())
			ingress.Close()
		}
	}
}

func proxyConnection(ingress net.Conn, customer, postgresPort string) {
	var conmap = make(map[int]net.Conn)

	dec := gob.NewDecoder(ingress)
	enc := gob.NewEncoder(ingress)

	messages := make(chan protocol.TransmissionUnit, 50)
	go MultiplexWriter(messages, enc, ingress, conmap)
	
	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)

		if err != nil {
			log.Printf("Read from client failed '%s', cleanup the proxy connection!\n", err.Error())
			// close all outgoing connections
			for key := range conmap {
				conmap[key].Close()
				delete(conmap, key)
			}
			ingress.Close()
			return
		}
		// log.Printf("Read TransmissionUnit %v\n", buff.Action)
		switch buff.Action {
		case protocol.Open:
			egress, err := getTargetConnection(customer, postgresPort)
			if err != nil {
				log.Printf("Error connecting to target")
			}
			conmap[buff.Id] = egress
			log.Printf("Connection #%d to db created, total #=%d\n", buff.Id, len(conmap))
			go pipe(egress, messages, buff.Id)
		case protocol.Close:
			if _, ok := conmap[buff.Id]; ok {
				conmap[buff.Id].Close()
				delete(conmap, buff.Id)
				log.Printf("Connection #%d closing, %d left\n", buff.Id, len(conmap))
			} else {
				log.Printf("Error finding the descriptor %d, %v\n", buff.Id, buff)
			}
		case protocol.Send:
			if sock, ok := conmap[buff.Id]; ok {
				_, err = sock.Write(buff.Data)
				bytesInLog <- len(buff.Data)
				//log.Printf("Write %d bytes to database at connection %d\n", len(buff.Data), buff.Id)
				if err != nil {
					log.Printf("Write to db error: %s\n", err.Error())
					conmap[buff.Id].Close()
					//delete(conmap, buff.Id)
				}
			} else {
				log.Printf("Error finding the descriptor %d, %v\n", buff.Id, buff)
			}
		}

	}
}
