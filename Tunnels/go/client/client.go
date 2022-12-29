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
	"encoding/gob"
	"encoding/json"
	"encoding/pem"
	"errors"
	"flag"
	"fmt"
	"dymium.com/client/ca"
	"dymium.com/client/content"
	"dymium.com/client/selfupdate"
	"dymium.com/client/types"
	"dymium.com/server/protocol"
	"github.com/apex/log"
	"github.com/apex/log/handlers/cli"

	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	_ "path/filepath"
	"runtime"
	"sync"
	"time"

	"github.com/dgrijalva/jwt-go"
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

var (
	MajorVersion    string
	MinorVersion    string
	ProtocolVersion string
)

func displayBuff(what string, buff []byte) {
	if len(buff) > 10 {
		head := buff[:6]
		tail := buff[len(buff)-6:]
		log.Debugf("%s head: %v, tail: %v", what, head, tail)
	} else {
		log.Debugf("%s buffer: %v", what, buff)
	}
}

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

func getTunnelInfo(customerid, portalurl string, forcenoupdate bool) (string, bool) {

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

	if !forcenoupdate {
		if ProtocolVersion < back.ProtocolVersion {
			log.Infof("The tunneling utility must be updated!")
			log.Infof("Go to %s/app/access?key=download for the download.", portalurl)
			os.Exit(1)
		} else {
			if ProtocolVersion >= back.ProtocolVersion {
				log.Infof("A new version %s.%s is available",
					back.ClientMajorVersion, back.ClientMinorVersion)
				log.Infof("at %s/app/access?key=download", portalurl)
			}

		}
	}
	return back.LoginURL, ProtocolVersion < back.ProtocolVersion
}

func pipe(ingress net.Conn, messages chan protocol.TransmissionUnit, conmap map[int]net.Conn, token string, id int) {
	out := protocol.TransmissionUnit{protocol.Open, id, []byte(token)}
	log.Debugf("Create proxy connection %d, number of connections %d", id, len(conmap))
	//write out result
	messages <- out

	for {
		buff := make([]byte, 0xffff)

		n, err := ingress.Read(buff)
		if err != nil {
			if err.Error() != "EOF" {
				log.Errorf("Read on loopback failed '%s'", err.Error())
			} else {
				log.Infof("Loopback connection closed")
			}
			ingress.Close()
			back := protocol.TransmissionUnit{protocol.Close, id, nil}
			messages <- back
			return
		}
		b := buff[:n]
		//fmt.Printf("Read %d bytes from local connection #%d", len(b), id)

		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
		messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, enc *gob.Encoder) {
	for {
		buff, ok := <-messages
		if !ok {
			log.Errorf("Error reading from SSL channel")
			close(messages)
			return
		}
		//fmt.Printf("Encode %d bytes into SSL channel", len(buff.Data))

		err := enc.Encode(buff)
		if err != nil {
			log.Errorf("Error writing into tunnel %s", err.Error())
		}
	}
}
func MultiplexReader(egress net.Conn, conmap map[int]net.Conn, dec *gob.Decoder, messages chan protocol.TransmissionUnit) {
	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)
		if err != nil {
			log.Errorf("Еrror reading from tunnel%s, closing...", err.Error())
			for key := range conmap {
				back := protocol.TransmissionUnit{protocol.Close, key, nil}
				messages <- back

				conmap[key].Close()
				delete(conmap, key)
			}
			egress.Close()
			os.Exit(1)
			return
		}
		switch buff.Action {
		case protocol.Close:
			sock, ok := conmap[buff.Id]
			if ok {
				sock.Close()
				delete(conmap, buff.Id)
			}
			log.Debugf("Closed connection %d, %d left", buff.Id, len(conmap))
		case protocol.Send:
			//fmt.Printf("Send %d bytes", len(buff.Data))
			sock, ok := conmap[buff.Id]
			if ok {
				_, err := sock.Write(buff.Data)
				//fmt.Printf("Wrote back to local socket to connection %d, %d bytes out of %d bytes", buff.Id, n, len(buff.Data))
				//displayBuff("Data: ", buff.Data )
				if err != nil {
					log.Errorf("Write to local socket error: %s, closing...", err.Error())
					//os.Exit(1)
					back := protocol.TransmissionUnit{protocol.Close, buff.Id, nil}
					sock.Close()
					delete(conmap, buff.Id)
					messages <- back
				}
			}
		}
	}
}

func runProxy(listener *net.TCPListener, back chan string, port int, token string) {
	defer wg.Done()

	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(ca.RootCApem))

	config := &tls.Config{
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{clientCert}}
	target := fmt.Sprintf("%s:%d", lbaddress, lbport)
	back <- fmt.Sprintf("Connect to %s", target)
	log.Debugf("tls.Dial to %s", target)
	egress, err := tls.Dial("tcp", target, config) // *Conn
	if err != nil {
		back <- err.Error()
		back <- "error"
		return
	}
	back <- "Connected successfully"
	log.Debugf("Wrote to back Connected")

	state := egress.ConnectionState()

	log.Debugf("Version: %x", state.Version)
	log.Debugf("HandshakeComplete: %t", state.HandshakeComplete)
	log.Debugf("DidResume: %t", state.DidResume)
	log.Debugf("CipherSuite: %x", state.CipherSuite)
	log.Debugf("NegotiatedProtocol: %s", state.NegotiatedProtocol)
	log.Debugf("NegotiatedProtocolIsMutual: %t", state.NegotiatedProtocolIsMutual)

	log.Debugf("Certificate chain:")

	for i, cert := range state.PeerCertificates {
		subject := cert.Subject
		issuer := cert.Issuer
		log.Debugf(" %d s:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", i, subject.Country, subject.Province, subject.Locality, subject.Organization, subject.OrganizationalUnit, subject.CommonName)
		log.Debugf("   i:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", issuer.Country, issuer.Province, issuer.Locality, issuer.Organization, issuer.OrganizationalUnit, issuer.CommonName)
	}

	var conmap = make(map[int]net.Conn)
	connectionCounter := 0
	dec := gob.NewDecoder(egress)
	enc := gob.NewEncoder(egress)
	messages := make(chan protocol.TransmissionUnit)
	go MultiplexWriter(messages, enc)
	go MultiplexReader(egress, conmap, dec, messages)

	back <- "end"

	for {
		ingress, err := listener.Accept() //*TCPConn
		if err != nil {
			log.Errorf("Error in Accept: %s", err.Error())
			panic(err)
		}

		conmap[connectionCounter] = ingress
		go pipe(ingress, messages, conmap, token, connectionCounter)
		connectionCounter++
	}
}
func doUpdate(portalUrl string) error {

	url := fmt.Sprintf("%sapi/downloadupdate?os=%s&arch=%s", portalUrl, runtime.GOOS, runtime.GOARCH)

	log.Infof("Downloading new version...")
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New(fmt.Sprintf("Error downloading update, status %d", resp.StatusCode))
	}
	ex, _ := os.Executable()
	log.Infof("Updating the client...")
	err = selfupdate.Apply(resp.Body, selfupdate.Options{ex, 0, nil, 0, ex + ".old"})
	if err != nil {
		log.Errorf("Error updating: %s", err.Error())
		if rerr := selfupdate.RollbackError(err); rerr != nil {
			log.Errorf("Failed to rollback from bad update: %s", rerr.Error())
		}
		// error handling
	} else {
		log.Infof("Utility successfully updated, restarting...")
	}
	return err
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
func main() {
	log.SetHandler(cli.New(os.Stderr))

	forcenoupdate := flag.Bool("r", false, "Don't update")
	forceupdate := flag.Bool("u", false, "Force update")
	verbose := flag.Bool("v", false, "Verbose")
	flag.StringVar(&customerid, "c", "", "Customer ID")
	flag.StringVar(&portalurl, "p", "", "Portal URL")
	flag.Parse()
	if !*forcenoupdate {
		log.Infof("Dymium secure tunnel, version %s.%s, protocol iteration %s", MajorVersion, MinorVersion, ProtocolVersion)
	}
	if *forcenoupdate && *forceupdate {
		log.Errorf("Can't force update and no update!")
		os.Exit(1)
	}

	wg.Add(1)

	if len(customerid) == 0 && len(portalurl) == 0 {
		if runtime.GOOS == "windows" {
			log.Infof("Usage: dymium.exe -c <customer ID> -p <portal URL> [-v]")
		} else {
			log.Infof("Usage: dymium -c <customer ID> -p <portal URL> [-v]")
		}
		os.Exit(1)
	}

	if *verbose {
		log.SetLevelFromString("Debug")
	} else {
		log.SetLevelFromString("Info")
	}

	loginURL /* needsUpdate */, _ := getTunnelInfo(customerid, portalurl, *forcenoupdate)

	/* disable self update for now
	if !*forcenoupdate {
		if *forceupdate || needsUpdate {
			// runtime.Breakpoint()
			err := doUpdate(portalurl)
			if err != nil {
				fmt.Printf("Error: %s", err.Error())
				os.Exit(1)
			}
			restart()
		}
	}
	*/

	err := browser.OpenURL(loginURL)
	if err != nil {
		log.Errorf("OpenURL failed: %s", err.Error())
	}
	p := mux.NewRouter()
	loggingMiddleware := func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// fmt.Printf("%s%s", r.Host, r.RequestURI)
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
			// fmt.Printf("Returned code: %s", code)
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
		var groups types.AuthorizationCodeResponse
		err = json.Unmarshal(body, &groups)
		if err != nil {
			log.Errorf("Error: %s", err.Error())
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
			log.Errorf("Error: token invalid, can't continue %s", err.Error())
			os.Exit(1)
		}

		listener, err := getListener(claim.Port, message)
		if err != nil {
			log.Errorf("Error: %s", err.Error())
		}

		go runProxy(listener, message, claim.Port, groups.Token)
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
			tail := fmt.Sprintf(content.Tail, portalurl, groups.Token, claim.Port)
			w.Write([]byte(tail))
			log.Infof("Ready to pass traffic securely!")
		}
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}

		if err := s.Shutdown(context.TODO()); err != nil {
			log.Errorf(err.Error()) // failure/timeout shutting down the server gracefully
			os.Exit(1)
		}

	}).Methods("GET")

	s.ListenAndServe()

	wg.Wait()

}
