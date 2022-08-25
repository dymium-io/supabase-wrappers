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



func getTargetConnection() (net.Conn, error) {
    target := "127.0.0.1:5432"
    return net.Dial("tcp", target)
}

func Server(address string, port int, certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte) {


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
    fmt.Printf("Connect to %s\n", connect)
    ln, err := tls.Listen("tcp", connect, config) 
    /*
    connect := fmt.Sprintf("%s:%d", address, port)


    ln, err := net.Listen("tcp", connect) 
    */
    if err != nil {
        log.Println(err)
        return
    }
    defer ln.Close()


fmt.Printf("Listen: %v\n", ln)
    for {
        ingress, err := ln.Accept()
        //fmt.Printf("accepted: %T\n", ingress)
        if err != nil {
            log.Println(err)
            continue
        }
		// get the underlying tls connection
		tlsConn, ok := ingress.(*tls.Conn)
		if !ok {
			fmt.Println("server: erm, this is not a tls conn")
			return
		}
		// perform handshake
		if err := tlsConn.Handshake(); err != nil {
			fmt.Println("client: error during handshake, error:", err)
			return
		}

		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
        for _, cert := range state.PeerCertificates {
            for _, name := range cert.DNSNames {
                fmt.Printf("%s\n", name)
            }
        }

        go proxyConnection(ingress)
    }
}

func pipe(egress net.Conn, messages chan protocol.TransmissionUnit, id int ) {
	buff := make([]byte, 0xffff)
	for {
		n, err := egress.Read(buff)
		if err != nil {
			fmt.Printf("Read failed '%s'\n", err.Error())
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
        fmt.Printf("Write %d bytes into SSL tunnel\n", len(buff.Data))
        enc.Encode(buff)
    }
}

func proxyConnection(ingress net.Conn) {
    var conmap = make( map[int] net.Conn )


    dec := gob.NewDecoder(ingress)
    enc := gob.NewEncoder(ingress)
    messages := make(chan protocol.TransmissionUnit)
    go MultiplexWriter(messages, enc)

	for {
        var buff protocol.TransmissionUnit
        err := dec.Decode(&buff)

		if err != nil {
			fmt.Printf("Read failed '%s'\n", err.Error())
			// close all outgoing connections
            for key := range conmap {
                conmap[key].Close()
                
                delete(conmap, key)
           
            }
			return
		}
        switch buff.Action {
        case protocol.Open:
            egress, err := getTargetConnection()
            if err != nil {
                fmt.Printf("Error connecting to target")
            }
            fmt.Printf("Connection #%d created\n", buff.Id)
            conmap[buff.Id] = egress
            go pipe(egress, messages, buff.Id)
        case protocol.Close:
            fmt.Printf("Connection #%d closing\n", buff.Id)
            conmap[buff.Id].Close()
            delete(conmap, buff.Id)
        case protocol.Send:
            fmt.Printf("Write %d bytes into connection #%d\n", len(buff.Data), buff.Id)
            conmap[buff.Id].Write(buff.Data)
        }

	}
}

