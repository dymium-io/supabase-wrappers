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
	"dymium.com/client/selfupdate"
	"dymium.com/client/types"
	"dymium.com/server/protocol"
	"encoding/gob"
	"encoding/json"
	"encoding/pem"
	"errors"
	"flag"
	"fmt"
	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	"github.com/pkg/browser"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	_ "path/filepath"
	"runtime"
	"sync"
	"time"
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
var connectionError = false
var wg sync.WaitGroup

var (
	MajorVersion    string
	MinorVersion    string
	ProtocolVersion string
)

func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	/*
		Failed to get userinfo: "+err.Error()
	*/

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
	fmt.Println("Generating certificate request...")
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
func restart() {

	ex, _ := os.Executable()

	procAttr := new(os.ProcAttr)
	procAttr.Files = []*os.File{nil, os.Stdout, os.Stderr}
	dir, _ := os.Getwd()
	procAttr.Dir = dir
	procAttr.Env = os.Environ()
	args := []string{}

	for _, v := range os.Args {
		if v == "-u" {
			continue
		}
		args = append(args, v)
	}
	args[0] = ex
	args = append(args, "-r")
	time.Sleep(time.Second)
	proc, err := os.StartProcess(ex, args, procAttr)
	if err != nil {
		fmt.Printf("StartProcess Error: %s\n", err.Error())
	}
	_, err = proc.Wait()
	if err != nil {
		fmt.Printf("proc.Wait error: %s\n", err.Error())
	}
	os.Exit(0)
}

func getTunnelInfo(customerid, portalurl string, forcenoupdate bool) (string, bool) {

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
	if(resp.StatusCode != 200) {
		fmt.Printf("Error retrieving the tunnel information. Please check your parameters.\n")
		os.Exit(1)
	}
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

	if !forcenoupdate {
		if ProtocolVersion < back.ProtocolVersion {
			fmt.Println("The tunneling utility must be updated!")
			fmt.Printf("Go to %s/app/access?key=download for the download.", portalurl)
			os.Exit(1)
		} else {
			if ProtocolVersion >= back.ProtocolVersion {
				fmt.Printf("A new version %s.%s is available\n",
					back.ClientMajorVersion, back.ClientMinorVersion)
				fmt.Printf("Go to %s/app/access?key=download for the download.", portalurl)					
			}

		}
	}
	return back.LoginURL, ProtocolVersion < back.ProtocolVersion
}

func pipe(ingress net.Conn, messages chan protocol.TransmissionUnit, conmap map[int]net.Conn, id int) {
	out := protocol.TransmissionUnit{protocol.Open, id, []byte{}}
	//write out result
	messages <- out

	buff := make([]byte, 0xffff)
	for {
		n, err := ingress.Read(buff)
		if err != nil {
			fmt.Printf("Read on loopback failed '%s'\n", err.Error())
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
		buff, ok := <-messages
		if !ok {
			fmt.Printf("Error reading from SSL channel\n")
			close(messages)
			return
		}
		fmt.Printf("Encode %d bytes into SSL channel\n", len(buff.Data))

		err := enc.Encode(buff)
		if(err != nil) {
			fmt.Printf("Error writing into tunnel %s\n", err.Error() )
		}
	}
}
func MultiplexReader(egress net.Conn, conmap map[int]net.Conn, dec *gob.Decoder) {
	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)
		if err != nil {
			fmt.Printf("Еrror reading from tunnel%s, closing...\n", err.Error() )
			for key := range conmap {
				conmap[key].Close()
				delete(conmap, key)
			}
			egress.Close()
			os.Exit(1)
			return
		}
		switch buff.Action {
		case protocol.Close:
			conmap[buff.Id].Close()
			delete(conmap, buff.Id)
		case protocol.Send:
			//fmt.Printf("Send %d bytes\n", len(buff.Data))
			sock, ok := conmap[buff.Id]
			if(ok) {
				_, err = sock.Write(buff.Data)
				if(err != nil) {
					fmt.Printf("Write to local socket error: %s, closing...", err.Error() )
					//os.Exit(1)
					sock.Close()
					delete(conmap, buff.Id)
				}
			}
		}
	}
}

func runProxy(listener *net.TCPListener, back chan string, token int) {
	defer wg.Done()

	/*
		addr, err := net.ResolveTCPAddr("tcp", fmt.Sprintf("127.0.0.1:%d", token))
		if err != nil {
			fmt.Printf("Error resolving address: %s\n", err.Error())
			panic(err)
		}
		listener, err := net.ListenTCP("tcp", addr)
		if err != nil {
			fmt.Printf("Error in ListenTCP: %s, can't continue\n", err.Error())
			back <- err.Error()
			os.Exit(1)
		}
	*/
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(RootCApem))

	config := &tls.Config{
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{clientCert}}
	target := fmt.Sprintf("%s:%d", lbaddress, lbport)
	back <- fmt.Sprintf("Connect to %s\n", target)
	// create a counterpart tls connection out
	egress, err := tls.Dial("tcp", target, config) // *Conn
	if err != nil {
		back <- err.Error()
		back <- "error"
		return
	}
	back <- "Connected successfully"
	//fmt.Printf("Wrote to back Connected")

	/* state  := egress.ConnectionState()

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
	*/

	var conmap = make(map[int]net.Conn)
	connectionCounter := 0
	dec := gob.NewDecoder(egress)
	enc := gob.NewEncoder(egress)
	messages := make(chan protocol.TransmissionUnit)
	go MultiplexWriter(messages, enc)
	go MultiplexReader(egress, conmap, dec)

	back <- "end"

	for {
		ingress, err := listener.Accept() //*TCPConn
		if err != nil {
			fmt.Printf("Error in Accept: %s\n", err.Error())
			panic(err)
		}

		conmap[connectionCounter] = ingress
		go pipe(ingress, messages, conmap, connectionCounter)
		connectionCounter++
	}
}
func doUpdate(portalUrl string) error {

	url := fmt.Sprintf("%sapi/downloadupdate?os=%s&arch=%s", portalUrl, runtime.GOOS, runtime.GOARCH)

	fmt.Println("Downloading new version...")
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New(fmt.Sprintf("Error downloading update, status %d", resp.StatusCode))
	}
	ex, _ := os.Executable()
	fmt.Println("Updating the client...")
	err = selfupdate.Apply(resp.Body, selfupdate.Options{ex, 0, nil, 0, ex + ".old"})
	if err != nil {
		fmt.Printf("Error updating: %s\n", err.Error())
		if rerr := selfupdate.RollbackError(err); rerr != nil {
			fmt.Println("Failed to rollback from bad update: %s", rerr.Error())
		}
		// error handling
	} else {
		fmt.Println("Utility successfully updated, restarting...")
	}
	return err
}
func getListener(port int, back chan string) (*net.TCPListener, error) {
	addr, err := net.ResolveTCPAddr("tcp", fmt.Sprintf("127.0.0.1:%d", port))
	if err != nil {
		fmt.Printf("Error resolving address: %s\n", err.Error())
		panic(err)
	}
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		fmt.Printf("Error in ListenTCP: %s, can't continue\n", err.Error())
		back <- err.Error()
		os.Exit(1)
	}
	fmt.Printf("Listener listening on port %d\n", port)
	return listener, err
}
func main() {
	forcenoupdate := flag.Bool("r", false, "Don't update")
	forceupdate := flag.Bool("u", false, "Force update")
	flag.StringVar(&customerid, "c", "", "Customer ID")
	flag.StringVar(&portalurl, "p", "", "Portal URL")
	flag.Parse()
	if !*forcenoupdate {
		fmt.Printf("Dymium secure tunnel, version %s.%s, protocol iteration %s\n", MajorVersion, MinorVersion, ProtocolVersion)
	}
	if *forcenoupdate && *forceupdate {
		fmt.Printf("Can't force update and no update!")
		os.Exit(1)
	}

	wg.Add(1)
	
	if len(customerid) == 0 && len(portalurl) == 0 {
		if runtime.GOOS == "windows" {
			fmt.Println("Usage: dymium.exe -c <customer ID> -p <portal URL>\n")
		} else {
			fmt.Println("Usage: dymium -c <customer ID> -p <portal URL>\n")
		}
		os.Exit(1)
	}

	loginURL, /* needsUpdate */ _ := getTunnelInfo(customerid, portalurl, *forcenoupdate)

	/* disable self update for now
	if !*forcenoupdate {
		if *forceupdate || needsUpdate {
			// runtime.Breakpoint()
			err := doUpdate(portalurl)
			if err != nil {
				fmt.Printf("Error: %s\n", err.Error())
				os.Exit(1)
			}
			restart()
		}
	}
	*/

	err := browser.OpenURL(loginURL)
	if err != nil {
		fmt.Printf("OpenURL failed: %s\n", err.Error())
	}
	p := mux.NewRouter()
	loggingMiddleware := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// fmt.Printf("%s%s\n", r.Host, r.RequestURI)
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
			// fmt.Printf("Returned code: %s\n", code)
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

		<div class="line">Authenticated!</div>
		<div class="line">Creating a short term client certificate...</div>

		`
		w.Write([]byte(obody))
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		csr, err := generateCSR(groups.Name, groups.Groups)

		sendCSR(csr, groups.Token)

		message := make(chan string)
		var claim types.Claims
		var p jwt.Parser
	
		_, _, err = p.ParseUnverified(groups.Token, &claim)
		if err != nil {
			fmt.Printf("Error: token invalid, can't continue %s\n", err.Error())
			os.Exit(1)
		}

		listener, err := getListener(claim.Port, message)
		if(err != nil) {
			fmt.Printf("Error: %s\n", err.Error())
		}

		go runProxy(listener, message, claim.Port)
		for {
			msg := <-message
			// fmt.Printf("read from channel: %s\n", msg)
			if msg == "end" {
				break
			}
			if msg == "error" {
				connectionError = true
				break
			}
			hbody := fmt.Sprintf(`
			<div class='line'>%s</div>`,
				msg)

			w.Write([]byte(hbody))
			if f, ok := w.(http.Flusher); ok {
				f.Flush()
			}
		}

		if connectionError {
			w.Write([]byte(content.ErrorTail))
			fmt.Println("Error detected, exiting...")
		} else {
			tail := fmt.Sprintf(content.Tail, portalurl, groups.Token, claim.Port)
			w.Write([]byte(tail))
			fmt.Println("Ready to pass traffic securely!")
		}
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}


		if err := s.Shutdown(context.TODO()); err != nil {
			panic(err) // failure/timeout shutting down the server gracefully
		}

	}).Methods("GET")

	s.ListenAndServe()

	wg.Wait()

}
