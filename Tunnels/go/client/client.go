//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package main

import (
	"bytes"
	"context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"dymium.com/client/content"
	"dymium.com/client/types"
	"encoding/json"
	"encoding/pem"
	"encoding/gob"
	"flag"
	"fmt"
	"github.com/gorilla/mux"
	"github.com/pkg/browser"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	"runtime"
    "dymium.com/server/protocol"	
)

const (
	csrPEMBlockType = "CERTIFICATE REQUEST"
	Nocache         = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
)

var certKey *rsa.PrivateKey
var clientCert tls.Certificate
var customerid string
var portalurl string
var lbaddress string
var lbport int

func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	/*
		Failed to get userinfo: "+err.Error()
	*/
	fmt.Printf("Error %s: %s\n", header, body)
	w.Header().Set("Cache-Control", Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(`<html>
		<head>
		<script>
		 !function() {
			window.location.href = "/app/error?header=` + url.QueryEscape(header) + `&body=` + url.QueryEscape(body) + `"
		 }()
		</script>
		</head>
		<body>Callback arrived</body>
		</html>`))

	return nil
}

// convert DER to PEM format
func pemCSR(derBytes []byte) []byte {
	pemBlock := &pem.Block{
		Type:    csrPEMBlockType,
		Headers: nil,
		Bytes:   derBytes,
	}
	out := pem.EncodeToMemory(pemBlock)
	return out
}
func generateCSR(name string, groups []string) ([]byte, error) {
	var err error
	certKey, err = rsa.GenerateKey(rand.Reader, 4096)
	if err != nil {
		return []byte{}, err
	}

	template := &x509.CertificateRequest{
		SignatureAlgorithm: x509.SHA256WithRSA,
		PublicKeyAlgorithm: x509.RSA,
		PublicKey:          &certKey.PublicKey,
		Subject:            pkix.Name{CommonName: name},
		DNSNames:           groups,
	}
	fmt.Println("Generating CSR")
	csrDER, err := x509.CreateCertificateRequest(rand.Reader, template, certKey)
	if err != nil {
		return []byte{}, err
	}
	out := pemCSR(csrDER)

	return out, nil
}

func sendCSR(csr []byte, token string) error {
	var outbody types.CertificateRequest
	outbody.Csr = string(csr)
	js, err := json.Marshal(outbody)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	urlStr := fmt.Sprintf("%sapi/getclientcertificate", portalurl)

	req, err := http.NewRequest("POST", urlStr, bytes.NewBuffer(js))
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	req.Header.Add("Content-Type", "application/json")
	req.Header.Add("Authorization", "Bearer "+token)

	fmt.Printf("token %s\n", token)
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	var back types.CSRResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	keyBytes := x509.MarshalPKCS1PrivateKey(certKey)

	pemBlock := &pem.Block{
		Type:    "RSA PRIVATE KEY",
		Headers: nil,
		Bytes:   keyBytes,
	}
	keyPem := pem.EncodeToMemory(pemBlock)

	clientCert, err = tls.X509KeyPair([]byte(back.Certificate), keyPem)
	if err != nil {
		fmt.Printf("Error in X509KeyPair: %s", err)
		os.Exit(1)
	}

	return nil
}

func getTunnelInfo(customerid, portalurl string) string {

	var outbody types.CustomerIDRequest
	outbody.Customerid = customerid
	js, err := json.Marshal(outbody)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	urlStr := fmt.Sprintf("%sapi/querytunnel", portalurl)
	resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js))

	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	var back types.CustomerIDResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}

	lbaddress = back.Lbaddress
	lbport = back.Lbport

	return back.LoginURL
}

func pipe(ingress net.Conn, messages chan protocol.TransmissionUnit, conmap map[int] net.Conn, id int ) {
	out := protocol.TransmissionUnit{protocol.Open, id, []byte{}}
	//write out result
	messages <- out

	buff := make([]byte, 0xffff)
	for {
		n, err := ingress.Read(buff)
		if err != nil {
			fmt.Printf("Read failed '%s'\n", err.Error())
			ingress.Close()
			return
		}
		b := buff[:n]
		fmt.Printf("Read %d bytes from connection #%d\n", len(b), id)

		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
        messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, enc *gob.Encoder) {
    for {
        buff, ok := <- messages 
        if(!ok) {
			fmt.Printf("Error reading from SSL channel\n")
            close(messages)
            return
        }
		fmt.Printf("Encode %d bytes into SSL channel\n", len(buff.Data) )

		enc.Encode(buff)
    }
}
func MultiplexReader(egress net.Conn, conmap map[int] net.Conn, dec *gob.Decoder) {
	for {
        var buff protocol.TransmissionUnit
        err := dec.Decode(&buff)
		if(err != nil) {
			fmt.Printf("In MultiplexReader - read error, cleaning up\n")
            for key := range conmap {
                conmap[key].Close()
                delete(conmap, key)
            }			
			egress.Close()
			return
		}
		switch buff.Action {
		case protocol.Close:
			fmt.Printf("In MultiplexReader - close connection %d\n", buff.Id)

			conmap[buff.Id].Close()
			delete(conmap, buff.Id)
		case protocol.Send:
			fmt.Printf("Send %d bytes\n", len(buff.Data) )
			conmap[buff.Id].Write(buff.Data)
		}
	}
}

func runProxy() {

	addr, err := net.ResolveTCPAddr("tcp", "127.0.0.1:24354")
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		panic(err)
	}

	if err != nil {
		panic(err)
	}
	config := &tls.Config{Certificates: []tls.Certificate{clientCert}}
	target := fmt.Sprintf("%s:%d", lbaddress, lbport)
	fmt.Printf("Connect to %s\n", target)
	// create a counterpart tls connection out
	egress, err := tls.Dial("tcp", target, config) // *Conn


	state := egress.ConnectionState()
	fmt.Printf("Version: %x\n", state.Version)
	fmt.Printf("HandshakeComplete: %t\n", state.HandshakeComplete)
	fmt.Printf("DidResume: %t\n", state.DidResume)
	fmt.Printf("CipherSuite: %x\n", state.CipherSuite)
	fmt.Printf("NegotiatedProtocol: %s\n", state.NegotiatedProtocol)
	fmt.Printf("NegotiatedProtocolIsMutual: %t\n", state.NegotiatedProtocolIsMutual)

	fmt.Print("Certificate chain:")
	for i, cert := range state.PeerCertificates {
		subject := cert.Subject
		issuer := cert.Issuer
		fmt.Printf(" %d s:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s\n", i, subject.Country, subject.Province, subject.Locality, subject.Organization, subject.OrganizationalUnit, subject.CommonName)
		fmt.Printf("   i:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s\n", issuer.Country, issuer.Province, issuer.Locality, issuer.Organization, issuer.OrganizationalUnit, issuer.CommonName)
	}
	
	var conmap = make( map[int] net.Conn )	
	connectionCounter := 0
    dec := gob.NewDecoder(egress)
    enc := gob.NewEncoder(egress)
    messages := make(chan protocol.TransmissionUnit)
    go MultiplexWriter(messages, enc)
	go MultiplexReader(egress, conmap, dec)
	for {
		ingress, err := listener.Accept() //*TCPConn
		if err != nil {
			panic(err)
		}
		
		//egress, err := net.Dial("tcp", target) // *Conn
		if err != nil {
			panic(err)
		}
		conmap[connectionCounter] = ingress
		go pipe(ingress, messages, conmap, connectionCounter)
		connectionCounter++
	}
}
func main() {

	flag.StringVar(&customerid, "c", "", "Customer ID")
	flag.StringVar(&portalurl, "p", "", "Portal URL")
	flag.Parse()

	if len(customerid) == 0 && len(portalurl) == 0 {
		if runtime.GOOS == "windows" {
			fmt.Println("Usage: tclient.exe -c <customer ID> -p <portal URL>")
		} else {
			fmt.Println("Usage: tclient -c <customer ID> -p <portal URL>")
		}
		os.Exit(1)
	}

	loginURL := getTunnelInfo(customerid, portalurl)
	browser.OpenURL(loginURL)

	p := mux.NewRouter()
	loggingMiddleware := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Printf("%s%s\n", r.Host, r.RequestURI)
			// Call the next handler, which can be another middleware in the chain, or the final handler.
			next.ServeHTTP(w, r)
		})
	}
	p.Use(loggingMiddleware)

	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		error := mux.Vars(r)["error"]
		error_description := mux.Vars(r)["error_description"]

		generateError(w, r, error, error_description)
	}).
		Queries("error", "{error}").
		Queries("error_description", "{error_description}").Methods("GET")
	s := http.Server{Addr: "127.0.0.1:63000", Handler: p}

	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		code := r.URL.Query().Get("code")
		if code == "" {
			generateError(w, r, "Error: ", r.URL.Query().Get("error_description"))
			return
		} else {
			fmt.Printf("Returned code: %s\n", code)
		}
		//w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")

		w.Write([]byte(content.Head))
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		var outbody types.AuthorizationCodeRequest
		outbody.Customerid = customerid
		outbody.Code = code

		js, err := json.Marshal(outbody)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}

		urlStr := fmt.Sprintf("%sapi/authenticatebycode", portalurl)
		resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js))

		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}
		defer resp.Body.Close()
		body, err := io.ReadAll(resp.Body)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}
		var groups types.AuthorizationCodeResponse
		err = json.Unmarshal(body, &groups)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}

		obody := `

		<h3 class="mt-5">Authenticated!</h3>
		<h3 class="mt-5">Creating a short term client certificate...</h3>

		`
		w.Write([]byte(obody))
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		csr, err := generateCSR(groups.Name, groups.Groups)

		sendCSR(csr, groups.Token)

		w.Write([]byte(content.Tail))
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		if err := s.Shutdown(context.TODO()); err != nil {
			panic(err) // failure/timeout shutting down the server gracefully
		}

	}).Methods("GET")

	s.ListenAndServe()
	fmt.Println("Done with the control channel")

	runProxy()
	/*
			cert, err := tls.X509KeyPair("certs/client.pem", "certs/client.key")
		    if err != nil {
		        log.Fatalf("server: loadkeys: %s", err)
		    }
		    config := tls.Config{Certificates: []tls.Certificate{cert}, InsecureSkipVerify: true}
		    conn, err := tls.Dial("tcp", "127.0.0.1:8000", &config)
		    if err != nil {
		        log.Fatalf("client: dial: %s", err)
		    }
		    defer conn.Close()
	*/

}
