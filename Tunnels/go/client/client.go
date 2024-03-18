// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
package main

import (
	"bufio"
	"bytes"
	"context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/json"
	"encoding/pem"
	"flag"
	"fmt"
	_ "net/http/pprof"
	"os/signal"
	"strings"
	"syscall"

	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	_ "path/filepath"
	"runtime"
	"sync"
	"html/template"
	"dymium.com/client/ca"
	"dymium.com/client/content"
	"dymium.com/client/installer"
	"dymium.com/client/types"
	"dymium.com/server/protocol"
	"github.com/apex/log"
	"github.com/apex/log/handlers/plaintext"
	"github.com/blang/semver/v4"

	"github.com/golang-jwt/jwt"
	"github.com/gorilla/mux"
	"github.com/pkg/browser"
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
var messagesCapacity = 16
var readBufferSize = 16 * 4096

/*
func displayBuff(what string, buff []byte) {
	if len(buff) > 100 {
		head := buff[:60]
		tail := buff[len(buff)-60:]
		log.Debugf("\n%s head: \n%s\ntail: \n%s\n", what, string(head), string(tail))
	} else {
		log.Debugf("\n%s buffer: \n%s\n", what, string(buff))
	}
}
*/
/*
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
		log.Errorf("StartProcess Error: %s", err.Error())
	}
	_, err = proc.Wait()
	if err != nil {
		log.Errorf("proc.Wait error: %s", err.Error())
	}
	os.Exit(0)
}
*/
const errorPageTemplateString = `
<html>
<head>
<script>
 !function() {
	window.location.href = "/app/error?header={{.HeaderEncoded}}&body={{.BodyEncoded}}"
 }()
</script>
</head>
<body>Callback arrived</body>
</html>`
type errorPageData struct {
	HeaderEncoded string
	BodyEncoded   string
}
func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	// Create a new template
	tmpl, err := template.New("errorPage").Parse(errorPageTemplateString)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return err
	}

	// Data for the template
	data := errorPageData{
		HeaderEncoded: url.QueryEscape(header),
		BodyEncoded:   url.QueryEscape(body),
	}

	// Use a bytes.Buffer to capture the output
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return err
	}


	w.Header().Set("Cache-Control", Nocache)
	w.Header().Set("Content-Type", "text/html")

	w.Write(buf.Bytes())

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
	log.Infof("Generating certificate request...")
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
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	urlStr := fmt.Sprintf("%sapi/getclientcertificate", portalurl)

	req, err := http.NewRequest("POST", urlStr, bytes.NewBuffer(js))
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}
	req.Header.Add("Content-Type", "application/json")
	req.Header.Add("Authorization", "Bearer "+token)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	var back types.CSRResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
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
		log.Errorf("Error in X509KeyPair: %s", err)
		os.Exit(1)
	}

	return nil
}

func getTunnelInfo(customerid, portalurl string, forcenoupdate bool, forceupdate bool) string {

	var outbody types.CustomerIDRequest
	outbody.Customerid = customerid
	js, err := json.Marshal(outbody)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	urlStr := fmt.Sprintf("%sapi/querytunnel", portalurl)
	resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js))

	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		log.Errorf("Error retrieving the tunnel information. Please check your parameters.")
		os.Exit(1)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	var back types.CustomerIDResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	lbaddress = back.Lbaddress
	lbport = back.Lbport

	vserver, _ := semver.Make(back.Version)
	vclient, _ := semver.Make(protocol.TunnelServerVersion)

	if !forcenoupdate {

		if vserver.Major > vclient.Major || ( (vserver.Major == vclient.Major) && 
			(vserver.Minor > vclient.Minor) ) {
			log.Infof("Server version incremented to %s, update itself!", back.Version)
			// DoUpdate(portal)
			os.Exit(0)
		} else {
			if vserver.GT(vclient) {
				log.Infof("Server version incremented to %s, update recommended", back.Version)
			} else {
				log.Infof("Server version %s, client is up to date", back.Version)
			}
		}
	}
	needsUpdate := vserver.Major > vclient.Major

	if !forcenoupdate {
		if forceupdate || needsUpdate {
			installer.UpdateInstaller(portalurl)
			log.Infof("Exit")
			os.Exit(0)
			log.Infof("Exit called")
		}
	}
	return back.LoginURL
}

func pipe(ingress net.Conn, messages chan *protocol.TransmissionUnit, conmap map[int]net.Conn, token string, id int, mu *sync.RWMutex) {
	out := protocol.TransmissionUnit{Action: protocol.Open, Id: id, Data: []byte(token)}

	mu.RLock()
	log.Debugf("Create proxy connection %d, number of connections %d", id, len(conmap))
	mu.RUnlock()
	//write out result
	messages <- &out
	arena := make([]byte, readBufferSize*(4+messagesCapacity))
	index := 0

	for {
		//buff := make([]byte, 16*4096)
		//buff := make([]byte, 64)
		buff := arena[index*readBufferSize : (index+1)*readBufferSize]
		index = (index + 1) % (4 + messagesCapacity)

		n, err := ingress.Read(buff)

		if err != nil {
			if err != io.EOF {
				s := err.Error()
				if strings.Contains(s, "use of closed network connection") {
				} else {
					log.Errorf("Read on loopback failed '%s'", err.Error())
				}
			} else {
				log.Infof("Connection %d closed by client", id)
			}
			ingress.Close()
			// log.Debugf("Read %d bytes from local connection #%d", n, id)
			back := protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
			messages <- &back
			return
		}
		b := buff[:n]
		//fmt.Printf("Read %d bytes from local connection #%d", len(b), id)

		out := protocol.TransmissionUnit{Action: protocol.Send, Id: id, Data: b}
		//write out result
		messages <- &out
	}
}

func MultiplexWriter(messages chan *protocol.TransmissionUnit, egress net.Conn) {
	for {
		buff, ok := <-messages
		if !ok {
			log.Errorf("Error reading from SSL channel")
			close(messages)
			return
		}
		// log.Debugf("Action %d, Encode %d bytes into SSL channel", buff.Action, len(buff.Data))
		err := protocol.WriteToTunnel(buff, egress)

		if err != nil {
			log.Errorf("Error writing into tunnel %s", err.Error())
		}
	}
}
func MultiplexReader(egress net.Conn, conmap map[int]net.Conn, messages chan *protocol.TransmissionUnit, mu *sync.RWMutex) {
	reader := bufio.NewReader(egress)
	st := make([]byte, protocol.ProtocolChunkSize)
	arena := make([]byte, readBufferSize*(4+messagesCapacity))
	index := 0

	for {
		var buff protocol.TransmissionUnit
		b := arena[index*readBufferSize : (index+1)*readBufferSize]
		index = (index + 1) % (4 + messagesCapacity)

		_, err := io.ReadFull(reader, st)

		if err == nil {
			err = protocol.GetBufferedTransmissionUnit(st, &buff, b, reader)

		}
		// log.Debugf("Read from tunnel %d bytes, action: %d, buffer len %d", n, buff.Action, len(buff.Data))
		if err != nil {
			if strings.Contains(err.Error(), "use of closed network connection") {
				log.Infof("Tunnel is closed, shutting down...")
			} else {
				log.Errorf("Еrror reading from tunnel %s, closing...", err.Error())
			}
			mu.Lock()
			for key := range conmap {
				back := protocol.TransmissionUnit{Action: protocol.Close, Id: key, Data: nil}
				messages <- &back
				conmap[key].Close()
				delete(conmap, key)
			}
			mu.Unlock()
			egress.Close()
			os.Exit(1)
			return
		}
		switch buff.Action {
		case protocol.Close:
			mu.RLock()
			sock, ok := conmap[buff.Id]
			mu.RUnlock()
			if ok {
				mu.Lock()
				sock.Close()
				delete(conmap, buff.Id)
				mu.Unlock()
			}
			mu.RLock()
			log.Infof("Closed connection %d, %d left", buff.Id, len(conmap))
			mu.RUnlock()
		case protocol.Send:
			mu.RLock()
			sock, ok := conmap[buff.Id]
			mu.RUnlock()
			//displayBuff("read:", buff.Data)
			if ok {
				_, err := sock.Write(buff.Data)

				if err != nil {
					s := err.Error()
					if strings.Contains(s, "use of closed network connection") {
					} else {
						log.Errorf("Write to local socket error: %s, closing...", s)
					}
					back := protocol.TransmissionUnit{Action: protocol.Close, Id: buff.Id, Data: nil}
					sock.Close()
					mu.Lock()
					delete(conmap, buff.Id)
					mu.Unlock()
					messages <- &back
				}
			}
		}
	}
}
func handleSignal(listener *net.TCPListener, egress *tls.Conn) {
	signalChan := make(chan os.Signal, 1)
	signal.Notify(signalChan, os.Interrupt, syscall.SIGTERM)

	<-signalChan
	log.Info("Received an interrupt, stopping the client.")
	egress.Close()
	listener.Close()
}

func runProxy(listener *net.TCPListener, back chan string, port int, token string) {
	defer wg.Done()

	caCertPool := x509.NewCertPool()
	for i := 0; i < len(ca.RootCApem); i++ {
		ok := caCertPool.AppendCertsFromPEM([]byte(ca.RootCApem[i]))
		log.Debugf("add ca #%d, status %t", i, ok)
	}

	config := &tls.Config{
		ServerName:   lbaddress,
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{clientCert}}
	target := fmt.Sprintf("%s:%d", lbaddress, lbport)
	back <- fmt.Sprintf("Connect to %s", target)
	log.Debugf("tls.Dial to %s", target)

	tcpConn, err := net.Dial("tcp", target)
	if err != nil {
		log.Errorf("Error dialing TCP: %s", err.Error())
		back <- err.Error()
		back <- "error"
		return
	}

	// Type assert to *net.TCPConn and set NoDelay
	tcpTCPConn, ok := tcpConn.(*net.TCPConn)
	if !ok {
		log.Errorf("Failed to assert net.Conn to *net.TCPConn")
		back <- err.Error()
		back <- "error"
		return
	}
	err = tcpTCPConn.SetNoDelay(true)
	if err != nil {
		log.Errorf("Error setting TCP_NODELAY: %s", err.Error())
		back <- err.Error()
		back <- "error"
		return
	}

	egress := tls.Client(tcpConn, config)

	// Perform the TLS handshake
	err = egress.Handshake()
	if err != nil {
		log.Errorf("TLS handshake failed: %s", err)
		back <- err.Error()
		back <- "error"
		return
	}
	/*
		egress, err := tls.Dial("tcp", target, config) // *Conn
		if err != nil {
			back <- err.Error()
			back <- "error"
			return
		}
	*/
	back <- "Connected successfully"
	log.Debugf("Wrote to back Connected")

	state := egress.ConnectionState()

	log.Debugf("Version: %x", state.Version)
	log.Debugf("HandshakeComplete: %t", state.HandshakeComplete)
	log.Debugf("DidResume: %t", state.DidResume)
	log.Debugf("CipherSuite: %x", state.CipherSuite)
	log.Debugf("NegotiatedProtocol: %s", state.NegotiatedProtocol)

	log.Debugf("Certificate chain:")

	for i, cert := range state.PeerCertificates {
		subject := cert.Subject
		issuer := cert.Issuer
		log.Debugf(" %d s:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", i, subject.Country, subject.Province, subject.Locality, subject.Organization, subject.OrganizationalUnit, subject.CommonName)
		log.Debugf("   i:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", issuer.Country, issuer.Province, issuer.Locality, issuer.Organization, issuer.OrganizationalUnit, issuer.CommonName)
	}

	var conmap = make(map[int]net.Conn)
	var mu sync.RWMutex

	connectionCounter := 0

	messages := make(chan *protocol.TransmissionUnit, messagesCapacity)
	go MultiplexWriter(messages, egress)
	go MultiplexReader(egress, conmap, messages, &mu)

	back <- "end"

	go handleSignal(listener, egress)
	setReuseAddr(listener)

	for {
		ingress, err := listener.Accept() //*TCPConn
		if err != nil {
			log.Errorf("Error in Accept: %s", err.Error())
			panic(err)
		}

		tcpConn, ok := ingress.(*net.TCPConn)
		if !ok {
			log.Errorf("Not a TCP connection")
			panic(err)
		}

		// Set TCP_NODELAY
		err = tcpConn.SetNoDelay(true)
		if err != nil {
			log.Errorf("Error setting TCP_NODELAY: %s", err.Error())
			panic(err)
		}

		mu.Lock()
		conmap[connectionCounter] = ingress
		mu.Unlock()
		go pipe(ingress, messages, conmap, token, connectionCounter, &mu)
		connectionCounter++
	}
}

func getListener(port int, back chan string) (*net.TCPListener, error) {
	addr, err := net.ResolveTCPAddr("tcp", fmt.Sprintf("127.0.0.1:%d", port))
	if err != nil {
		log.Errorf("Error resolving address: %s", err.Error())
		panic(err)
	}
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		log.Errorf("Error in ListenTCP: %s, can't continue", err.Error())
		back <- err.Error()
		os.Exit(1)
	}
	log.Infof("Listener listening on port %d", port)
	return listener, err
}
func getCustomerGroups(customerid, code string) types.AuthorizationCodeResponse {
	var groups types.AuthorizationCodeResponse
	var outbody types.AuthorizationCodeRequest
	outbody.Customerid = customerid
	outbody.Code = code

	js, err := json.Marshal(outbody)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	urlStr := fmt.Sprintf("%sapi/authenticatebycode", portalurl)
	resp, err := http.Post(urlStr, "application/json", bytes.NewBuffer(js))

	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}

	err = json.Unmarshal(body, &groups)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}
	return groups
}
func waitForConnection(message chan string, w http.ResponseWriter, port int, groups types.AuthorizationCodeResponse) {
	for {
		msg := <-message

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
		log.Errorf("Error detected, exiting...")
	} else {
		tail := fmt.Sprintf(content.Tail, portalurl, groups.Token, port)
		w.Write([]byte(tail))
		log.Infof("Ready to pass traffic securely!")
	}
	if f, ok := w.(http.Flusher); ok {
		f.Flush()
	}

}
func help(customerid, portalurl string) {
	if len(customerid) == 0 && len(portalurl) == 0 {
		if runtime.GOOS == "windows" {
			log.Infof("Usage: dymium.exe -c <customer ID> -p <portal URL> [-v]")
		} else {
			log.Infof("Usage: dymium -c <customer ID> -p <portal URL> [-v]")
		}
		os.Exit(1)
	}
}
func setLogLevel(verbose *bool) {
	if *verbose {
		log.SetLevel(log.DebugLevel)
	} else {
		log.SetLevel(log.InfoLevel)
	}
}
func printAuthenticatedFeedback(w http.ResponseWriter) {
	obody := `
	<div class="line">Authenticated!</div>
	<div class="line">Creating a short term client certificate...</div>
	`
	w.Write([]byte(obody))
	if f, ok := w.(http.Flusher); ok {
		f.Flush()
	}
}

func main() {
	log.SetHandler(plaintext.New(os.Stderr))
	/*
		go func() {
			http.ListenAndServe("localhost:6060", nil)
		}()
	*/
	forcenoupdate := flag.Bool("r", false, "Don't update")
	forceupdate := flag.Bool("u", false, "Force update")
	verbose := flag.Bool("v", false, "Verbose")
	flag.StringVar(&customerid, "c", "", "Customer ID")
	flag.StringVar(&portalurl, "p", "", "Portal URL")
	flag.Parse()

	if !*forcenoupdate {
		log.Infof("Dymium secure tunnel, version %s", protocol.TunnelServerVersion)
	}
	if *forcenoupdate && *forceupdate {
		log.Errorf("Can't force update and no update!")
		os.Exit(1)
	}

	wg.Add(1)
	help(customerid, portalurl)
	setLogLevel(verbose)

	loginURL := getTunnelInfo(customerid, portalurl, *forcenoupdate, *forceupdate)

	err := browser.OpenURL(loginURL)
	if err != nil {
		log.Errorf("OpenURL failed, probably you don't have a browser on your system: %s", err.Error())
		os.Exit(1)
	}
	p := mux.NewRouter()

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
		}
		w.Header().Set("Content-Type", "text/html")

		w.Write([]byte(content.Head))
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		groups := getCustomerGroups(customerid, code)
		printAuthenticatedFeedback(w)

		csr, err := generateCSR(groups.Name, groups.Groups)
		if err != nil {
			log.Errorf("Error: token invalid, can't continue %s", err.Error())
			os.Exit(1)
		}
		sendCSR(csr, groups.Token)

		var claim types.Claims
		var p jwt.Parser
		_, _, err = p.ParseUnverified(groups.Token, &claim) // only informational
		if err != nil {
			log.Errorf("Error: token invalid, can't continue %s", err.Error())
			os.Exit(1)
		}

		message := make(chan string)
		listener, err := getListener(claim.Port, message)
		if err != nil {
			log.Errorf("Error: %s", err.Error())
		}

		go runProxy(listener, message, claim.Port, groups.Token)
		waitForConnection(message, w, claim.Port, groups)
		if err := s.Shutdown(context.TODO()); err != nil {
			log.Errorf(err.Error()) // failure/timeout shutting down the server gracefully
			os.Exit(1)
		}
	}).Methods("GET")

	s.ListenAndServe()

	wg.Wait() // this process will be killed by a signal

}
