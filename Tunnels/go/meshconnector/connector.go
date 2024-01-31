// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
package main

import (
	"bytes"
	_ "context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/json"
	"encoding/pem"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	"dymium.com/dymium/log"
	"dymium.com/meshconnector/ca"
	"dymium.com/meshconnector/selfupdate"
	"dymium.com/meshconnector/types"
	"dymium.com/server/protocol"
	"github.com/blang/semver/v4"
	"github.com/gorilla/mux"
)

const (
	csrPEMBlockType = "CERTIFICATE REQUEST"
	Nocache         = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
)

var certKey *rsa.PrivateKey
var clientCert tls.Certificate
var interrupted = false

type Virtcon struct {
	sock    net.Conn
	tenant  string
	inbound chan []byte
}

var messagesCapacity = 16
var readBufferSize = 16 * 4096

var readCount int32 = 0

func DoUpdate(portalUrl string) error {

	url := fmt.Sprintf("%sapi/downloadconnectorupdate?os=%s&arch=%s", portalUrl, runtime.GOOS, runtime.GOARCH)

	log.Infof("Downloading new version from %s...", url)
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New(fmt.Sprintf("Error downloading update, status %d", resp.StatusCode))
	}
	ex, _ := os.Executable()
	log.Infof("Updating %s...", ex)
	err = selfupdate.Apply(resp.Body, selfupdate.Options{ex, 0, nil, 0, "meshconnector." + protocol.MeshServerVersion + ".bak"})
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
		_, _ = http.Get("http://localhost:" + port + "/restart")
	}
	return err
}
func restart() {

	ex := selfupdate.GetEx()

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
	/*
		_, err = proc.Wait()
		if err != nil {
			fmt.Printf("proc.Wait error: %s\n", err.Error())
		}
	*/

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

	err := http.ListenAndServe("localhost:"+port, p)
	log.Errorf("ListenAndServe exited, %s", err.Error())
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
	log.InfoTenantf(customer, "Generating certificate request...")
	csrDER, err := x509.CreateCertificateRequest(rand.Reader, template, certKey)
	if err != nil {
		return []byte{}, err
	}
	out := pemCSR(csrDER)

	return out, nil
}

func ReaderFromDb(conmap map[int]*Virtcon,
	messages chan *protocol.TransmissionUnit, id int,
	token string, mu *sync.RWMutex, customer string) {

	mu.RLock()
	l := len(conmap)
	mu.RUnlock()
	//runtime.GC()
	log.InfoTenantf(customer, "Creating connection #%d to db at %s, total #=%d", id, token, l)

	dbside, err := net.Dial("tcp", token)

	if err != nil {
		log.ErrorTenantf(customer, "Error connecting to target:  %s, send close back", err.Error())
		messages <- &protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
		return
	} else {
		log.InfoTenantf(customer, "Created connection #%d to db at %s, total #=%d", id, token, l)
	}

	mu.Lock()
	conn, ok := conmap[id] // todo process ok
	if ok {
		conn.sock = dbside
	}
	mu.Unlock()
	if !ok {
		log.ErrorTenantf(customer, "Error finding the descriptor %d in pipe", id)
		messages <- &protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
		return
	}
	go func() {
		for {
			inbound := <-conn.inbound
			//log.InfoTenantf(customer, "Written for #%d, %d bytes", id, len(inbound))
			if conn != nil && conn.sock != nil {
				conn.sock.Write(inbound)
			} else {
				return
			}
		}
	}()
	// start a goroutine that writes into socket from conn.inbound
	arena := make([]byte, readBufferSize*(4+messagesCapacity))
	index := 0
	for {
		buff := arena[index*readBufferSize : (index+1)*readBufferSize]
		index = (index + 1) % (4 + messagesCapacity)
		n, err := dbside.Read(buff)
		mu.RLock()
		conn, ok := conmap[id]
		mu.RUnlock()
		if err != nil {
			if strings.Contains(err.Error(), "use of closed network connection") {
				// no op
			} else {
				if ok {
					if err != io.EOF {
						log.ErrorTenantf(conn.tenant, "Db read failed '%s', id:%d", err.Error(), id)
					}
				} else {
					if err != io.EOF {
						log.Errorf("Db read failed '%s', id:%d", err.Error(), id)
					}
				}
			}
			mu.Lock()
			if conn != nil && conn.sock != nil {
				conn.sock.Close()
				conn.sock = nil
			}
			mu.Unlock()
			dbside.Close()
			out := protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
			messages <- &out

			return
		}
		b := buff[:n]

		out := protocol.TransmissionUnit{Action: protocol.Send, Id: id, Data: b}
		//write out result
		messages <- &out
	}
}

func WriterToService(messages chan *protocol.TransmissionUnit,
	serviceside net.Conn) {
	for {
		buff, ok := <-messages
		if !ok {
			close(messages)
			return
		}

		err := protocol.WriteToTunnel(buff, serviceside)
		if err != nil {
			log.Errorf("Error in encoder: %s", err.Error())
			if serviceside != nil {
				serviceside.Close()
			}
		}
	}
}
func Pinger(serviceside net.Conn, messages chan *protocol.TransmissionUnit, wake chan int) {
	var ping protocol.TransmissionUnit
	// log.Infof("In Pinger")
	ping.Action = protocol.Ping
	counter := 0
	for {
		select {
		case <-wake:
			log.Debug("Pinger exited")
			return
		case <-time.After(30 * time.Second):
		}
		if counter != 0 && counter%4 == 0 {
			if atomic.LoadInt32(&readCount) == 0 {
				// Do something if val is 0
				log.Error("Server inactivity detected, close")
				serviceside.Close()
				continue
			} else {
				atomic.StoreInt32(&readCount, 0)
			}
		}
		counter++

		// log.Debug("Send Ping ")
		messages <- &ping

	}
}
func InformParentOfUpdate() {
	port := os.Getenv("HEALTHPORT")
	http.Get(":" + port + "/upgrade")
	log.Info("Overseer contacted")
}

func ReaderServiceSide(serviceside *tls.Conn, customer string, messages chan *protocol.TransmissionUnit) {
	updateStatus("active")

	var conmap = make(map[int]*Virtcon)
	var mu sync.RWMutex
	defer serviceside.Close()

	wake := make(chan int, 1)
	go Pinger(serviceside, messages, wake)

	st := make([]byte, protocol.ProtocolChunkSize)
	for {
		var buff protocol.TransmissionUnit
		_, err := protocol.ReadFull(serviceside, st, 9)

		protocol.GetTransmissionUnit(st, &buff, serviceside)

		if err != nil {
			if err == io.EOF {
				log.Errorf("Customer %s, read from connector failed '%s', cleanup the proxy connection!",
					customer, err.Error())
			} else {
				if !interrupted {
					log.Errorf("Server closed connection, cleanup the proxy")
				} else {
					log.Info("Doing final cleanup")
				}
			}
			// close all outgoing connections
			wake <- 1
			log.Debug("Sent wake to pinger")
			mu.Lock()
			for key := range conmap {
				if conmap[key].sock != nil {
					conmap[key].sock.Close()
					conmap[key].sock = nil
				}
			}
			for key := range conmap {
				delete(conmap, key)
			}
			mu.Unlock()
			log.Debug("Cleaned up connection map")
			return
		}
		atomic.AddInt32(&readCount, 1)

		switch buff.Action {
		case protocol.Error:
			log.Errorf("Server returned error: %s", string(buff.Data))
			mu.Lock()
			for key := range conmap {
				if conmap[key].sock != nil {
					conmap[key].sock.Close()
					conmap[key].sock = nil
				}
				delete(conmap, key)
			}
			mu.Unlock()
			wake <- 0
			return
		case protocol.Ping:
			// log.Info("Ping received")

		case protocol.Open:
			conn := &Virtcon{}
			conn.tenant = customer
			conn.inbound = make(chan []byte, 50)
			mu.Lock()
			conmap[buff.Id] = conn
			mu.Unlock()

			go ReaderFromDb(conmap, messages, buff.Id, string(buff.Data), &mu, customer)
		case protocol.Close:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			l := len(conmap)
			mu.RUnlock()
			if ok {
				log.InfoTenantf(conn.tenant, "Connection #%d closing, %d left", buff.Id, l-1)

				mu.Lock()
				if conn != nil && conn.sock != nil {
					conn.sock.Close()
				}
				conn.sock = nil
				delete(conmap, buff.Id)
				mu.Unlock()
				log.InfoTenantf(conn.tenant, "Connection #%d closed, %d left", buff.Id, l-1)
			} else {
				log.Errorf("Error finding the descriptor %d in Close", buff.Id)
			}
		case protocol.Send:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if ok {
				conn.inbound <- buff.Data
			} else {
				log.Errorf("Error finding the descriptor %d in Send", buff.Id)
			}
		}
	}

}
func CreateTunnel(tunnelserver string, clientCert *tls.Certificate) (*tls.Conn, error) {
	caCertPool := x509.NewCertPool()
	for i := 0; i < len(ca.RootCApem); i++ {

		ok := caCertPool.AppendCertsFromPEM([]byte(ca.RootCApem[i]))
		if !ok {
			log.Errorf("Add certificate authority #%d, status %t", i, ok)
		}
	}

	host, _, _ := net.SplitHostPort(tunnelserver)
	config := &tls.Config{
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{*clientCert},
		ServerName:   host,
	}

	tcpConn, err := net.Dial("tcp", tunnelserver)
	if err != nil {
		log.Errorf("Error dialing TCP: %s", err.Error())
		return nil, err
	}

	// Type assert to *net.TCPConn and set NoDelay
	tcpTCPConn, ok := tcpConn.(*net.TCPConn)
	if !ok {
		log.Errorf("Failed to assert net.Conn to *net.TCPConn")
		return nil, err
	}
	err = tcpTCPConn.SetNoDelay(true)
	if err != nil {
		log.Errorf("Error setting TCP_NODELAY: %s", err.Error())
		return nil, err
	}

	dbside := tls.Client(tcpConn, config)

	if err != nil {
		log.Errorf("Error connecting to %s: %s", tunnelserver, err.Error())
		return nil, err
	}

	log.Info("Connected to Dymium!")
	state := dbside.ConnectionState()

	log.Debugf("Certificate chain:")

	for i, cert := range state.PeerCertificates {
		subject := cert.Subject
		issuer := cert.Issuer
		log.Debugf(" %d s:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", i, subject.Country, subject.Province, subject.Locality, subject.Organization, subject.OrganizationalUnit, subject.CommonName)
		log.Debugf("   i:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", issuer.Country, issuer.Province, issuer.Locality, issuer.Organization, issuer.OrganizationalUnit, issuer.CommonName)
	}
	log.Info("Tunnel created")
	return dbside, nil
}
func handleSignal(serviceside *tls.Conn, x chan int) {
	signalChan := make(chan os.Signal, 1)
	signal.Notify(signalChan, os.Interrupt, syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP)
	select {
	case <-signalChan:
		var overseer bool
		overseer = os.Getenv("WORKER") != "on"

		if overseer {
			log.Info("Received an interrupt in overseer, stopping...")
			updateStatus("configured")

			os.Exit(0)
		} else {
			log.Info("Received an interrupt in worker, stopping...")
		}
		interrupted = true

		serviceside.Close()
	case <-x:
		return
	}

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
	urlStr := fmt.Sprintf("%sapi/connectorstatus", portal)

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
func ConnectToService() {
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
	urlStr := fmt.Sprintf("%sapi/getconnectorcertificate", portal)

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
		log.Error("Check connector configuration in the portal, it may be wrong or absent.")
		return
	}

	if err != nil {
		log.Errorf("Error reading from %s: %s", portal, err.Error())
		return
	}

	defer resp.Body.Close()
	var back types.CSRResponse
	err = json.Unmarshal(body, &back)
	if err != nil {
		log.Errorf("Error unmarshaling response body: %s", err.Error())
		return
	}
	version := string(back.Version)
	if v := os.Getenv("VERSION"); v != "" {
		version = v
		log.Debugf("Imposed version %s", version)
	}

	vserver, _ := semver.Make(version)
	vclient, _ := semver.Make(protocol.MeshServerVersion)
	if vserver.Major > vclient.Major || ( (vserver.Major == vclient.Major) && 
	(vserver.Minor > vclient.Minor) ) {
		log.Infof("Server version incremented to %s, current: %s, update itself!", version, protocol.MeshServerVersion)
		DoUpdate(portal)
		os.Exit(0)
	} else {
		log.Infof("Server version: %s, client %s is up to date", version, protocol.MeshServerVersion)
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
		if len(c.DNSNames) == 0 {
			log.Error("No tunnels are returned, connector parameters are probably misconfigured")
		}
	} else {
		log.Infof("error parsing %s\n%s", e.Error(), back.Certificate)
	}

	tunnelserver := os.Getenv("TUNNELSERVER")

	serviceside, err := CreateTunnel(tunnelserver, &clientCert)
	if err != nil {
		log.Infof("CreateTunnel failed, %s", err.Error())
		return
	}
	x := make(chan int, 1)
	go handleSignal(serviceside, x)
	messages := make(chan *protocol.TransmissionUnit, messagesCapacity)
	go WriterToService(messages, serviceside)

	ReaderServiceSide(serviceside, customer, messages)
	x <- 1
	log.Debug("Woke up signal handler")
	updateStatus("configured")
}
func main() {
	log.Init("connector")

	if "" == os.Getenv("WORKER") {
		log.Infof("Overseer started, version %s", protocol.MeshServerVersion)
		x := make(chan int, 1)
		go handleSignal(nil, x)
		restart()
		health()
	} else {

		for {
			log.Infof("Worker started, version %s", protocol.MeshServerVersion)
			ConnectToService()
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
