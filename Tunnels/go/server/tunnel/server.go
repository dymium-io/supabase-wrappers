package tunnel

import (
	"crypto/tls"
	"crypto/x509"
	"dymium.com/server/protocol"
	"encoding/gob"
	"fmt"
	"log"
	"net"
	"os"
)

var iprecords []net.IP
var ipindex = 0

func getTargetConnection(customer, postgresPort string) (net.Conn, error) {
	log.Printf("in getTargetConnection, len(iprecords): %d\n", len(iprecords))
	index := ipindex % len(iprecords)
	ipindex = (ipindex + 1) % len(iprecords)

	target := iprecords[index].String() + ":" + postgresPort
	log.Printf("target: %s\n", target)
	return net.Dial("tcp", target)
}

func Server(address string, port int, customer, postgressDomain, postgresPort string, certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte) {
	var err error
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
	buff := make([]byte, 0xffff)
	for {
		n, err := egress.Read(buff)
		if err != nil {
			log.Printf("Db read failed '%s', id:%d\n", err.Error(), id)
			egress.Close()
			return
		}
		b := buff[:n]
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
			log.Printf("Read from client failed '%s', cleanup the pipe!\n", err.Error())
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
			log.Printf("Connection #%d created\n", buff.Id)
			conmap[buff.Id] = egress
			go pipe(egress,  messages, buff.Id)
		case protocol.Close:
			log.Printf("Connection #%d closing\n", buff.Id)
            if _, ok := conmap[buff.Id]; ok {
                conmap[buff.Id].Close()
                delete(conmap, buff.Id)
            } else {
                log.Printf("Error finding the descriptor %d, %v\n", buff.Id, buff)
            }
		case protocol.Send:
			if _, ok := conmap[buff.Id]; ok {
				_, err = conmap[buff.Id].Write(buff.Data)
				if err != nil {
					log.Printf("Write to db error: %s\n", err.Error())
					conmap[buff.Id].Close()
				}
			} else { 
                log.Printf("Error finding the descriptor %d, %v\n", buff.Id, buff)
            }
		}

	}
}
