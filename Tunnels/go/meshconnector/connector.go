//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package main

import (
	"bytes"
	_ "context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/gob"
	"encoding/json"
	"encoding/pem"
	"sync"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"time"
	"strings"
	"dymium.com/dymium/log"
	"dymium.com/meshconnector/ca"
	"dymium.com/meshconnector/types"
	"dymium.com/server/protocol"
	"github.com/gorilla/mux"
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

var (
	MajorVersion    string
	MinorVersion    string
	ProtocolVersion string
)

type Virtcon struct {
	sock            net.Conn
	tenant          string
	accumDownstream int
	totalDownstream int
	accumUpstream   int
	totalUpstream   int
}


func health() {
	p := mux.NewRouter()

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

func displayBuff(what string, buff []byte) {
	if len(buff) > 10 {
		head := buff[:6]
		tail := buff[len(buff)-6:]
		log.Debugf("%s head: %v, tail: %v", what, head, tail)
	} else {
		log.Debugf("%s buffer: %v", what, buff)
	}
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


func pipe(conmap map[int]*Virtcon, egress net.Conn, 
	messages chan protocol.TransmissionUnit, id int, 
	token string, mu *sync.RWMutex) {

	for {
		buff := make([]byte, 0xffff)
		n, err := egress.Read(buff)
		mu.RLock()
		conn, ok := conmap[id]
		mu.RUnlock()
		if err != nil {
			if strings.Contains(err.Error(), "use of closed network connection") {
				// no op
			} else {	
				if ok {
					es := err.Error()
					if( !strings.Contains(es, "EOF")) {
						log.ErrorTenantf(conn.tenant, "Db read failed '%s', id:%d", err.Error(), id)
					}
				} else {
					es := err.Error()
					if( !strings.Contains(es, "EOF")) {
						log.Errorf("Db read failed '%s', id:%d", err.Error(), id)
					}
				}
			}
			egress.Close()
			out := protocol.TransmissionUnit{protocol.Close, id, nil}
			messages <- out
	
			return
		}
		b := buff[:n]

		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
		messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, 
	enc *gob.Encoder, ingress net.Conn) {
	for {
		buff, ok := <-messages
		if !ok {
			close(messages)
			return
		}

		err := enc.Encode(buff)
		if err != nil {
			log.Errorf("Error in encoder: %s", err.Error())
			ingress.Close()
		}
	}
}
func PassTraffic(ingress *tls.Conn, customer string) {
	var conmap = make(map[int]*Virtcon)
	var mu sync.RWMutex

	dec := gob.NewDecoder(ingress)
	enc := gob.NewEncoder(ingress)

	messages := make(chan protocol.TransmissionUnit, 50)
	go MultiplexWriter(messages, enc, ingress)

	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)

		if err != nil {
			log.Errorf("Customer %s, read from client failed '%s', cleanup the proxy connection!", 
				customer, err.Error())
			// close all outgoing connections
			mu.Lock()
			for key := range conmap {
				conmap[key].sock.Close()
				delete(conmap, key)
			}
			mu.Unlock()
			ingress.Close()
			return
		}

		switch buff.Action {
		case protocol.Open:

			conn := &Virtcon{}
			conn.tenant = customer
			mu.RLock()
			l := len(conmap)
			mu.RUnlock()
			log.InfoTenantf(conn.tenant, "Creating connection #%d to db at %s, total #=%d", buff.Id, string(buff.Data), l)

			egress, err := net.Dial("tcp", string(buff.Data))
			if err != nil {
				log.ErrorTenantf(conn.tenant, "Error connecting to target :  %s, send close back", err.Error())
				messages <- protocol.TransmissionUnit{protocol.Close, buff.Id, nil}
				return
			}
			conn.sock = egress
			mu.Lock()
			conmap[buff.Id] = conn
			mu.Unlock()
			go pipe(conmap, egress, messages, buff.Id, string(buff.Data), &mu)
		case protocol.Close:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			l := len(conmap)
			mu.RUnlock()
			if  ok {
				log.InfoTenantf(conn.tenant, "Connection #%d closing, %d left", buff.Id, l-1)
				conn.sock.Close()
				mu.Lock()
				delete(conmap, buff.Id)
				mu.Unlock()
			} else {
				log.Errorf("Error finding the descriptor %d, %v", buff.Id, buff)
			}
		case protocol.Send:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if ok {
				_, err = conn.sock.Write(buff.Data)

				if err != nil {
					log.ErrorTenantf(conn.tenant, "Write to db error: %s", err.Error())
					conn.sock.Close()
				}
			} else {
				log.Errorf("Error finding the descriptor %d, %v", buff.Id, buff)
			}
		}
	}

}
func CreateTunnel(tunnelserver string, clientCert *tls.Certificate) (*tls.Conn, error) {
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(ca.RootCApem))

	config := &tls.Config{
		RootCAs:      caCertPool,
		Certificates: []tls.Certificate{*clientCert}}

	log.Infof("tls.Dial to %s", tunnelserver)
	egress, err := tls.Dial("tcp", tunnelserver, config) // *Conn
	if err != nil {
		log.Errorf("Error connecting to %s: %s", tunnelserver, err.Error() )

		return nil, err
	}

	log.Info("Connected to Dymium!")

	state := egress.ConnectionState()
/*
	log.Debugf("Version: %x", state.Version)
	log.Debugf("HandshakeComplete: %t", state.HandshakeComplete)
	log.Debugf("DidResume: %t", state.DidResume)
	log.Debugf("CipherSuite: %x", state.CipherSuite)
	log.Debugf("NegotiatedProtocol: %s", state.NegotiatedProtocol)
	log.Debugf("NegotiatedProtocolIsMutual: %t", state.NegotiatedProtocolIsMutual)
*/
	log.Debugf("Certificate chain:")

	for i, cert := range state.PeerCertificates {
		subject := cert.Subject
		issuer := cert.Issuer
		log.Debugf(" %d s:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", i, subject.Country, subject.Province, subject.Locality, subject.Organization, subject.OrganizationalUnit, subject.CommonName)
		log.Debugf("   i:/C=%v/ST=%v/L=%v/O=%v/OU=%v/CN=%s", issuer.Country, issuer.Province, issuer.Locality, issuer.Organization, issuer.OrganizationalUnit, issuer.CommonName)
	}

	return egress, nil
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
	if resp.StatusCode != 200 {
		log.Errorf("Invalid response %d from %s", resp.StatusCode, portal)
		return

	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
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

	keyBytes := x509.MarshalPKCS1PrivateKey(certKey)

	pemBlock := &pem.Block{
		Type:    "RSA PRIVATE KEY",
		Headers: nil,
		Bytes:   keyBytes,
	}
	keyPem := pem.EncodeToMemory(pemBlock)
	log.Infof("client cert: %s", back.Certificate)


	clientCert, err = tls.X509KeyPair([]byte(back.Certificate), keyPem)
	if err != nil {
		log.Errorf("Error in X509KeyPair: %s", err)
		os.Exit(1)
	}
	
	tunnelserver := os.Getenv("TUNNELSERVER")

	ingress, err := CreateTunnel(tunnelserver, &clientCert)
	if err != nil {
		return
	}
	PassTraffic(ingress, customer)
}
func main() {
	if "true" != os.Getenv("LOCAL_ENVIRONMENT") {
			go health()
	}
	log.Init("connector")
	
	// TODO:
	// UPDATE
	for {
		DoConnect()
		log.Infof("Wait 20 sec before retrying...")
		time.Sleep(20 * time.Second)
		log.Infof("Reconnecting...")
	}
}