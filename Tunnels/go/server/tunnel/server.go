package tunnel

import (
    "log"
    "crypto/tls"
    "net"
    "fmt"
    "crypto/x509"
    "encoding/gob"
    "dymium.com/server/protocol"
    
)


var iprecords []net.IP
var ipindex = 0
func getTargetConnection(customer string) (net.Conn, error) {
    log.Printf("in getTargetConnection, len(iprecords): %d\n", len(iprecords))
    index := ipindex % len(iprecords)
    ipindex = (ipindex + 1) % len(iprecords)
 
    target := iprecords[index].String() + ":5432"
    log.Printf("target: %s\n", target)
    return net.Dial("tcp", target)
}

func Server(address string, port int, customer string, certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte) {
	iprecords, _ = net.LookupIP(customer + ".guardian.local")
	for _, ip := range iprecords {
		log.Printf("db endpoint: %s\n", ip)
	}

    cer, err := tls.X509KeyPair(certPEMBlock, keyPEMBlock ) 
    if err != nil {
        log.Println(err)
        return
    }

    caCertPool := x509.NewCertPool()
    caCertPool.AppendCertsFromPEM(caCert)

    config := &tls.Config{
        Certificates: []tls.Certificate{cer},
        ClientCAs: caCertPool,
        ClientAuth: tls.RequireAndVerifyClientCert,
    }
    connect := fmt.Sprintf("%s:%d", address, port)
    log.Printf("TLS listen on %s\n", connect)
    ln, err := tls.Listen("tcp", connect, config) 

    if err != nil {
        log.Printf("Error in tls.Listen: %s\n",err.Error() )
        return
    }
    defer ln.Close()


    for {
        ingress, err := ln.Accept()
 
        if err != nil {
            log.Printf("Error in tls.Accept: %s\n", err.Error() )
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
			log.Printf("client: error during handshake, error: %s\n", err.Error() )
			continue
		}

		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
        for _, cert := range state.PeerCertificates {
            for _, name := range cert.DNSNames {
                log.Printf("Group: %s\n", name)
            }
        }

        go proxyConnection(ingress, customer)
    }
}

func pipe(egress net.Conn, messages chan protocol.TransmissionUnit, id int ) {
	buff := make([]byte, 0xffff)
	for {
		n, err := egress.Read(buff)
		if err != nil {
			log.Printf("Read failed '%s'\n", err.Error())
			egress.Close()
			return
		}
		b := buff[:n]
        out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
        messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, enc *gob.Encoder) {
    for {
        buff, ok := <- messages 
        if(!ok) {
            close(messages)
            return
        }
        log.Printf("Write %d bytes into SSL tunnel\n", len(buff.Data))
        enc.Encode(buff)
    }
}

func proxyConnection(ingress net.Conn, customer string) {
    var conmap = make( map[int] net.Conn )


    dec := gob.NewDecoder(ingress)
    enc := gob.NewEncoder(ingress)
    messages := make(chan protocol.TransmissionUnit)
    go MultiplexWriter(messages, enc)

	for {
        var buff protocol.TransmissionUnit
        err := dec.Decode(&buff)

		if err != nil {
			log.Printf("Read failed '%s'\n", err.Error())
			// close all outgoing connections
            for key := range conmap {
                conmap[key].Close()
                
                delete(conmap, key)
           
            }
			return
		}
        log.Printf("Read TransmissionUnit %v\n", buff.Action)
        switch buff.Action {
        case protocol.Open:
            log.Println("open")
            egress, err := getTargetConnection(customer)
            if err != nil {
                log.Printf("Error connecting to target")
            }
            log.Printf("Connection #%d created\n", buff.Id)
            conmap[buff.Id] = egress
            go pipe(egress, messages, buff.Id)
        case protocol.Close:
            log.Printf("Connection #%d closing\n", buff.Id)
            conmap[buff.Id].Close()
            delete(conmap, buff.Id)
        case protocol.Send:
            log.Printf("Write %d bytes into connection #%d\n", len(buff.Data), buff.Id)
            conmap[buff.Id].Write(buff.Data)
        }

	}
}

