package tunnel

import (
    "log"
    "crypto/tls"
    "net"
    "fmt"
    "crypto/x509"
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
fmt.Printf("cert: %v\n\n", cer)

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


        egress, err := getTargetConnection()
        go proxyConnection(ingress, egress)
    }
}

func pipe(inc, outc net.Conn) {
	buff := make([]byte, 0xffff)
	for {
		n, err := inc.Read(buff)
		if err != nil {
			fmt.Printf("Read failed '%s'\n", err.Error())
			outc.Close()
			return
		}
		b := buff[:n]
		//write out result
		n, err = outc.Write(b)
		if err != nil {
			fmt.Printf("Write failed '%s'\n", err.Error())
			inc.Close()
			return
		}
		fmt.Printf("transfered %d bytes, %s\n", n, string(b))

	}
}
func proxyConnection(ingress, egress net.Conn) {
	go pipe(ingress, egress)
	go pipe(egress, ingress)
}
