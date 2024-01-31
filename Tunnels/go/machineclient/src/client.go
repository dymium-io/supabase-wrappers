// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
package main

import (
	"bufio"
	"bytes"
	_ "context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"time"
	"runtime"
	"encoding/json"
	"encoding/pem"
	"fmt"
	"os/signal"
	"strconv"
	"strings"
	"syscall"

	"dymium.com/client/ca"
	_ "dymium.com/client/ca"
	_ "dymium.com/client/sockopts"
	"dymium.com/client/types"
	"dymium.com/meshconnector/selfupdate"
	"dymium.com/server/protocol"
	"github.com/apex/log"
	"github.com/blang/semver/v4"
	"github.com/gorilla/mux"

	"io"
	"net"
	"net/http"
	_ "net/url"
	"os"
	_ "path/filepath"
	"sync"
	_ "time"
)

const (
	csrPEMBlockType = "CERTIFICATE REQUEST"
	Nocache         = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
)

var certKey *rsa.PrivateKey
var clientCert tls.Certificate

var messagesCapacity = 16
var readBufferSize = 16 * 4096

var interrupted = false

func displayBuff(what string, buff []byte) {
	if len(buff) > 10 {
		head := buff[:6]
		tail := buff[len(buff)-6:]
		log.Debugf("%s head: %v, tail: %v", what, head, tail)
	} else {
		log.Debugf("%s buffer: %v", what, buff)
	}
}

func pipe(ingress net.Conn, messages chan *protocol.TransmissionUnit, conmap map[int]net.Conn, token string, id int, mu *sync.RWMutex) {
	out := protocol.TransmissionUnit{Action: protocol.Open, Id: id, Data: []byte(token)}

	mu.RLock()
	log.Debugf("Create proxy connection %d, number of connections %d", id, len(conmap))
	mu.RUnlock()
	//write out result
	messages <- &out
	arena := make([]byte, readBufferSize*(2*messagesCapacity))
	index := 0

	for {
		// buff := make([]byte, 4096)
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
			back := protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
			messages <- &back
			return
		}
		b := buff[:n]
		log.Debugf("Read %d bytes from local connection #%d", len(b), id)

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
		log.Debugf("Encode %d bytes into SSL channel", len(buff.Data))

		err := protocol.WriteToTunnel(buff, egress)
		if err != nil {
			log.Errorf("Error writing into tunnel %s", err.Error())
		}
	}
}

func MultiplexReader(egress net.Conn, listener *net.TCPListener, conmap map[int]net.Conn, messages chan *protocol.TransmissionUnit, mu *sync.RWMutex) {
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

		if err != nil {
			if strings.Contains(err.Error(), "use of closed network connection") {
				log.Infof("Tunnel is closed, shutting down...")
			} else {
				log.Errorf("Еrror reading from tunnel %s, closing...", err.Error())
			}
			mu.Lock()
			log.Debug("Closing all connections")
			for key := range conmap {
				back := protocol.TransmissionUnit{Action: protocol.Close, Id: key, Data: nil}
				log.Debugf("Send close to %d", key)
				messages <- &back
				conmap[key].Close()
				delete(conmap, key)
			}
			mu.Unlock()
			egress.Close()
			listener.Close()

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

func runProxy(listener *net.TCPListener, back chan string, port int, token string) {

	caCertPool := x509.NewCertPool()
	for i := 0; i < len(ca.RootCApem); i++ {
		ok := caCertPool.AppendCertsFromPEM([]byte(ca.RootCApem[i]))
		log.Debugf("add ca #%d, status %t", i, ok)
	}
	log.Infof("Number of CA certificates: %d", len(ca.RootCApem))
	target := os.Getenv("TUNNELSERVER")
	host, _, _ := net.SplitHostPort(target)
	config := &tls.Config{
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{clientCert},
		ServerName:   host,
	}
	log.Info("TLS configuration created")

	//back <- fmt.Sprintf("Connect to %s", target)
	log.Debugf("tls.Dial to %s", target)
	//egress, err := tls.Dial("tcp", target, config) // *Conn

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

	if err != nil {
		log.Errorf("Error in tls.Dial: %s", err.Error())
		// back <- err.Error()
		// back <- "error"
		return
	}
	// back <- "Connected successfully"
	// log.Debugf("Wrote to back Connected")

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

	messages := make(chan *protocol.TransmissionUnit)
	go MultiplexWriter(messages, egress)
	go MultiplexReader(egress, listener, conmap, messages, &mu)

	//back <- "end"

	go handleSignal(listener, egress)

	//sockopts.SetReuseAddr(listener)

	for {
		ingress, err := listener.Accept() //*TCPConn
		if err != nil {
			log.Errorf("Error in Accept: %s", err.Error())
			return
		}
		mu.Lock()
		conmap[connectionCounter] = ingress
		mu.Unlock()
		go pipe(ingress, messages, conmap, token, connectionCounter, &mu)
		connectionCounter++
	}
}

func getListener(port int, back chan string) (*net.TCPListener, error) {
	addr, err := net.ResolveTCPAddr("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		log.Errorf("Error resolving address: %s", err.Error())
		panic(err)
	}
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		log.Errorf("Error in ListenTCP: %s, can't continue", err.Error())
		// back <- err.Error()
		os.Exit(1)
	}
	log.Infof("Listener listening on port %d", port)
	return listener, err
}

func handleSignal(listener *net.TCPListener, egress *tls.Conn) {
	signalChan := make(chan os.Signal, 1)
	signal.Notify(signalChan, os.Interrupt, syscall.SIGTERM)

	<-signalChan
	log.Info("Received an interrupt, stopping the worker.")
	interrupted = true
	egress.Close()
	listener.Close()
	os.Exit(0)
}

func pemCSR(derBytes []byte) []byte {
	pemBlock := &pem.Block{
		Type:    csrPEMBlockType,
		Headers: nil,
		Bytes:   derBytes,
	}
	out := pem.EncodeToMemory(pemBlock)
	return out
}

func generateCSR(customer string) ([]byte, error) {
	var err error
	certKey, err = rsa.GenerateKey(rand.Reader, 4096)
	if err != nil {
		return []byte{}, err
	}
	log.Infof("GenerateCSR: customer %s", customer)
	template := &x509.CertificateRequest{
		SignatureAlgorithm: x509.SHA256WithRSA,
		PublicKeyAlgorithm: x509.RSA,
		PublicKey:          &certKey.PublicKey,
		Subject:            pkix.Name{CommonName: customer},
		DNSNames:           []string{},
	}
	log.Info("Generating certificate request...")
	csrDER, err := x509.CreateCertificateRequest(rand.Reader, template, certKey)
	if err != nil {
		return []byte{}, err
	}
	out := pemCSR(csrDER)

	return out, nil
}

func DoUpdate(portalUrl string) error {

	url := fmt.Sprintf("%sapi/downloadmachineclientupdate?os=%s&arch=%s", portalUrl, runtime.GOOS, runtime.GOARCH)

	log.Infof("Downloading new version from %s...", url)
	resp, err := http.Get(url)
	
	if err != nil {
		log.Errorf("Error downloading update: %s", err.Error())
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		log.Errorf("Error downloading update, status %d", resp.StatusCode)
		return fmt.Errorf("Error downloading update, status %d", resp.StatusCode)
	}
	ex, _ := os.Executable()
	log.Infof("Updating %s...", ex)
	err = selfupdate.Apply(resp.Body, selfupdate.Options{ex, 0, nil, 0, "machineclient." + protocol.MeshServerVersion + ".bak"})
	if err != nil {
		log.Infof("Error updating: %s", err.Error())
		if rerr := selfupdate.RollbackError(err); rerr != nil {
			log.Infof("Failed to rollback from bad update: %s", rerr.Error())
		}
		// error handling
	} else {
		log.Info("Utility successfully updated, restarting...")
		port := os.Getenv("HEALTHPORT")
		if port == "" {
			port = "80"
		}
		_, err = http.Get("http://localhost:" + port + "/restart")
		if err != nil {
			log.Errorf("Error restarting: %s", err.Error() )
		} else {
			log.Info("http://localhost:" + port + "/restart called successfully")
		}
	}
	return err
}

func DoConnect() {
	// -------------------
	customer := os.Getenv("CUSTOMER")

	csr, err := generateCSR(customer)
	if err != nil {
		log.Errorf("Error generating CSR %s", err.Error())
		os.Exit(1)
	}
	// Get Certificate from the portal
	var crs types.CertificateRequestWithSecret
	crs.Csr = string(csr)
	crs.Customer = customer
	crs.Secret = os.Getenv("SECRET")
	crs.Key = os.Getenv("KEY")

	js, err := json.Marshal(crs)
	if err != nil {
		log.Errorf("Fatal Error: %s", err.Error())
		os.Exit(1)
	}

	portal := os.Getenv("PORTAL")
	urlStr := fmt.Sprintf("%sapi/getmachineclientcertificate", portal)

	req, err := http.NewRequest("POST", urlStr, bytes.NewBuffer(js))
	if err != nil {
		log.Errorf("Error connecting to %s: %s", portal, err.Error())
		return // retry later
	}
	req.Header.Add("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		log.Errorf("Error connecting to %s: %s", portal, err.Error())
		return
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)

	if resp.StatusCode != 200 {
		log.Errorf("Failed to authenticate, return code: %d from %s: %s", resp.StatusCode, urlStr, strings.Replace(string(body), "\n", "\\n", -1))
		log.Error("Check machine client configuration in the portal, it may be wrong or absent.")
		return
	}

	if err != nil {
		log.Errorf("Error reading from %s: %s", portal, err.Error())
		return
	}

	defer resp.Body.Close()
	var back types.MachineCSRResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		log.Errorf("Error unmarshaling response body: %s", err.Error())
		return
	}
	token := string(back.Jwt)

	vserver, _ := semver.Make(back.Version)
	vclient, _ := semver.Make(protocol.TunnelServerVersion)
	if vserver.Major > vclient.Major || ( (vserver.Major == vclient.Major) && 
	(vserver.Minor > vclient.Minor) ) {
		log.Infof("Server version incremented to %s, update itself!", back.Version)
		err := DoUpdate(portal)
		if err == nil {
			os.Exit(0)
		}
	} else {
		if vserver.GT(vclient) {
			log.Infof("Server version incremented to %s, update recommended", back.Version)
		} else {
			log.Infof("Server version %s, client is up to date", back.Version)
		}
	}

	keyBytes := x509.MarshalPKCS1PrivateKey(certKey)

	pemBlock := &pem.Block{
		Type:    "RSA PRIVATE KEY",
		Headers: nil,
		Bytes:   keyBytes,
	}
	keyPem := pem.EncodeToMemory(pemBlock)
	//log.Infof("key: %s", string(keyPem))
	clientCert, err = tls.X509KeyPair([]byte(back.Certificate), keyPem)
	if err != nil {
		log.Errorf("Error in X509KeyPair: %s", err)
		os.Exit(1)
	}

	block, _ := pem.Decode([]byte(back.Certificate))
	if block == nil {
		log.Errorf("failed to parse certificate PEM")
	}

	c, e := x509.ParseCertificate(block.Bytes)
	if e == nil {
		log.Info("cert parsed")
		for _, nm := range c.DNSNames {
			log.Infof("Tunnel: %s", nm)
		}
	} else {
		log.Infof("error parsing %s\n%s", e.Error(), back.Certificate)
	}
	sport := os.Getenv("LISTENER_PORT")
	if sport == "" {
		sport = "5432"
	}
	port, _ := strconv.Atoi(sport)

	message := make(chan string, 10)
	listener, err := getListener(port, message)
	
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		return
	} else {
		defer listener.Close()
	}


	runProxy(listener, message, port, token)
	// waitForConnection(message)
}
func health() {
	p := mux.NewRouter()

	p.HandleFunc("/restart", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		w.Header().Set("Content-Type", "text/html")
		log.Info("restart request from child")
		restart()
		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	p.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		w.Header().Set("Content-Type", "text/html")

		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	p.HandleFunc("/healthshellcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		w.Header().Set("Content-Type", "text/html")

		io.WriteString(w, "<html><body>OK</body></html>")

	}).Methods("GET")

	port := os.Getenv("HEALTHPORT")
	if port == "" {
		port = "80"
	}
	log.Infof("Listen for health on :%s", port)
	http.ListenAndServe(":"+port, p)

}
func updateStatus(updown string) {
	// Get Certificate from the portal
	var status types.SetConnectorStatus
	customer := os.Getenv("CUSTOMER")
	secret := os.Getenv("SECRET")
	key := os.Getenv("KEY")
	status.Customer = customer
	status.Secret = secret
	status.Key = key
	status.Status = updown

	js, err := json.Marshal(status)
	if err != nil {
		log.Errorf("Fatal Error: %s", err.Error())
		os.Exit(1)
	}

	portal := os.Getenv("PORTAL")
	urlStr := fmt.Sprintf("%sapi/machineclientstatus", portal)

	req, _ := http.NewRequest("POST", urlStr, bytes.NewBuffer(js))
	req.Header.Add("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		log.Errorf("Error connecting to %s: %s", portal, err.Error())
		return
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Update Status failed: %s", err.Error())
		return
	}
	if resp.StatusCode != 200 {
		log.Errorf("Update Status failed: Invalid response %d from %s: %s", resp.StatusCode, urlStr, string(body))
		return
	}
	log.Infof("Status updated %s", updown)

}
func restart() {

	ex := "./machineclient"

	procAttr := new(os.ProcAttr)
	procAttr.Files = []*os.File{nil, os.Stdout, os.Stderr}
	dir, _ := os.Getwd()
	procAttr.Dir = dir
	procAttr.Env = os.Environ()
	args := []string{}

	procAttr.Env = append(procAttr.Env, "WORKER=on")

	for _, v := range os.Args {
		args = append(args, v)
	}
	args[0] = ex
	args = append(args, "-r")
	time.Sleep(time.Second)
	log.Infof("Restart process %s", ex)
	_, err := os.StartProcess(ex, args, procAttr)
	if err != nil {
		log.Errorf("StartProcess Error: %s", err.Error())
	}
}

func handleOverseerSignal(serviceside *tls.Conn, x chan int) {
	signalChan := make(chan os.Signal, 1)
	signal.Notify(signalChan, os.Interrupt, syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP)
	select {
	case <-signalChan:
		var overseer bool
		overseer = os.Getenv("WORKER") != "on"
	
		if overseer {
			log.Info("Received an interrupt in overseer, stopping...")
			updateStatus("configured")
			log.Debug("Exit overseer")
			os.Exit(0)
		} else {
			log.Info("Received an interrupt in worker, stopping...")
		}
		interrupted = true
		if serviceside != nil {
			serviceside.Close()
		}
	case <-x:
		log.Debug("Received a message in overseer signal handler, exiting...")
		return
	}

}

func main() {
	verbose := os.Getenv("LOG_LEVEL")
	if verbose != "" {
		log.SetLevelFromString(verbose)
	}

	if "" == os.Getenv("WORKER") {
		log.Infof("Overseer started, version %s", protocol.TunnelServerVersion)
		x := make(chan int, 1)
		go handleOverseerSignal(nil, x)
		restart()
		health()
	} else {

		for {
			log.Infof("Worker started, version %s", protocol.TunnelServerVersion)
			DoConnect()
			if interrupted {
				log.Debug("Exiting on interrupt")
				break
			}

			log.Infof("Wait 20 sec before retrying...")
			time.Sleep(20 * time.Second)
			if interrupted {
				break
			}
			log.Infof("Reconnecting...")
		}
	}
}
