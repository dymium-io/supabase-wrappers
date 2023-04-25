package tunnel

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/gob"
	"encoding/pem"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"sync"
	"sync/atomic"
	"time"
	"dymium.com/dymium/log"
	"dymium.com/server/gotypes"
	"dymium.com/server/protocol"
	"github.com/dgrijalva/jwt-go"
	"github.com/redis/go-redis/v9"
)
var rdb *redis.Client
var ctx = context.Background()
type Virtcon struct {
	sock    net.Conn
	tenant  string
	email   string
	session string
	groups  []string
	roles   []string
	accumDownstream int64
	totalDownstream int64
	accumUpstream int64
	totalUpstream int64
}
func (x *Virtcon) LogDownstream(bytes int, final bool) {
	atomic.AddInt64(&x.accumDownstream, int64(bytes))
	atomic.AddInt64(&x.totalDownstream, int64(bytes))
	accumDownstream := atomic.LoadInt64(&x.accumDownstream)
	if( (accumDownstream > 10000 || final) && accumDownstream > 0) {
		sl := fmt.Sprintf("%d", x.accumDownstream)
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "Downstream, sent #%d bytes", []string{sl}, x.accumDownstream)
		atomic.StoreInt64(&x.accumDownstream, 0)
	}
	if(final) {
		sl := fmt.Sprintf("%d", atomic.LoadInt64(&x.totalDownstream) )
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "End of connection, total downstream traffic %d bytes", []string{sl}, atomic.LoadInt64(&x.totalDownstream))
	}
}
func (x *Virtcon) LogUpstream(bytes int, final bool) {
	atomic.AddInt64(&x.accumUpstream, int64(bytes))
	atomic.AddInt64(&x.totalUpstream, int64(bytes))
	accumUpstream := atomic.LoadInt64(&x.accumUpstream)
	if( (accumUpstream > 10000 || final) && accumUpstream > 0) {
		sl := fmt.Sprintf("%d", x.accumUpstream)
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "Upstream, sent #%d bytes", []string{sl}, x.accumUpstream)
		atomic.StoreInt64(&x.accumUpstream, 0)
	}
	if(final) {
		sl := fmt.Sprintf("%d", atomic.LoadInt64(&x.totalUpstream) )
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "End of connection, total upstream traffic %d bytes", []string{sl}, atomic.LoadInt64(&x.totalUpstream))
	}
}

var iprecords []net.IP
var ipindex = 0
var bytesInLog = make(chan int, 128)
var bytesOutLog = make(chan int, 128)

func logBandwidth(customer string) {
	bytesIn := 0
	bytesOut := 0
	for {
		select {
		case _bytesIn := <-bytesInLog:
			bytesIn += _bytesIn
			//log.Debugf("bytesIn=%d", bytesIn)
		case _bytesOut := <-bytesOutLog:
			bytesOut += _bytesOut
			//log.Debugf("bytesOut=%d", bytesOut)
		case <-time.After(30 * time.Second):
			if bytesIn != 0 || bytesOut != 0 {
				_, err := rdb.Pipelined(ctx, func(pipe redis.Pipeliner) error {
					pipe.IncrBy(ctx, customer+":bytesin", int64(bytesIn))
					pipe.IncrBy(ctx, customer+":bytesout", int64(bytesOut))
					pipe.IncrBy(ctx, "$:bytesin", int64(bytesIn))
					pipe.IncrBy(ctx, "$:bytesout", int64(bytesOut))

					pipe.Expire(ctx, "$:bytesout", time.Hour)
					return nil
				})

				if err != nil {
					log.Debugf("Error saving bytes to redis: %s", err.Error())
				}
			}
			bytesIn = 0
			bytesOut = 0
		}
	}
}

func initRedis(redisAddress, redisPort, redisPassword string)  {
	o := &redis.Options{
		Addr: redisAddress + ":" + redisPort,		
	}
	if redisPassword != "" {
		o.Password = redisPassword
	}
	rdb = redis.NewClient(o)

}
func displayBuff(what string, buff []byte) {
	if len(buff) > 14 {
		head := buff[:6]
		tail := buff[len(buff)-6:]
		log.Debugf("%s head: %v, tail: %v", what, head, tail)
	} else {
		log.Debugf("%s buffer: %v", what, buff)
	}
}
func getTargetConnection(customer, postgresPort string) (net.Conn, error) {
	index := ipindex % len(iprecords)
	ipindex = (ipindex + 1) % len(iprecords)

	target := iprecords[index].String() + ":" + postgresPort
	log.Infof("For customer %s, target: %s", customer, target)
	return net.Dial("tcp", target)
}

func Server(address string, port int, customer, postgressDomain, postgresPort string,
	certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte,
	redisAddress, redisPort, redisPassword string) {
	var err error
	var pkey []byte

	initRedis(redisAddress, redisPort, redisPassword)

	go logBandwidth(customer)
	targetHost := customer + postgressDomain
	iprecords, err = net.LookupIP(targetHost)
	log.Infof("Target postgress: %s", targetHost)
	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
	}
	for _, ip := range iprecords {
		log.Debugf("db endpoint: %s", ip)
	}

	v, _ := pem.Decode(keyPEMBlock)
	if v != nil {

		if v.Type == "RSA PRIVATE KEY" {
			if x509.IsEncryptedPEMBlock(v) {
				pkey, _ = x509.DecryptPEMBlock(v, []byte(passphrase))
				pkey = pem.EncodeToMemory(&pem.Block{
					Type:  v.Type,
					Bytes: pkey,
				})
			} else {
				pkey = pem.EncodeToMemory(v)
			}
		}
	}

	cer, err := tls.X509KeyPair(certPEMBlock, pkey)
	if err != nil {
		log.Errorf("Error decoding KeyPair", err.Error())
		os.Exit(1)
	}

	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM(caCert)

	config := &tls.Config{
		Certificates: []tls.Certificate{cer},
		ClientCAs:    caCertPool,
		ClientAuth:   tls.RequireAndVerifyClientCert,
	}
	connect := fmt.Sprintf("%s:%d", address, port)
	log.Debugf("TLS listen on %s", connect)
	ln, err := tls.Listen("tcp", connect, config)

	if err != nil {
		log.Errorf("Error in tls.Listen: %s", err.Error())
		os.Exit(1)
	}
	defer ln.Close()

	for {
		ingress, err := ln.Accept()

		if err != nil {
			log.Errorf("Error in tls.Accept: %s", err.Error())
			continue
		}
		// get the underlying tls connection
		tlsConn, ok := ingress.(*tls.Conn)
		if !ok {
			log.Errorf("server: erm, this is not a tls conn")
			continue
		}
		// perform handshake
		if err := tlsConn.Handshake(); err != nil {
			log.Errorf("client: error during handshake, error: %s", err.Error())
			continue
		}

		// get connection state and print some stuff
		state := tlsConn.ConnectionState()
		for _, cert := range state.PeerCertificates {
			for _, name := range cert.DNSNames {
				log.Debugf("Group: %s", name)
			}
		}

		go proxyConnection(ingress, customer, postgresPort)
	}
}

func pipe(conmap map[int]*Virtcon, egress net.Conn, messages chan protocol.TransmissionUnit, id int, token string, mu *sync.RWMutex) {

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
					log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, 
							"Db read failed '%s', id:%d", err.Error(), id)
				} else {
					log.Errorf("Db read failed '%s', id:%d", err.Error(), id)
				}
			}
			egress.Close()
			out := protocol.TransmissionUnit{protocol.Close, id, nil}
			messages <- out
			if ok {
				conn.LogDownstream(0, true)
			}
			return
		}
		b := buff[:n]
		bytesOutLog <- n


		if ok {
			conn.LogDownstream(n, false)
			//log.InfoUserArrayf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Downstream, sent #%d bytes", []string{sl}, n)
		}

		//displayBuff("Read from db ", b)
		//log.Printf("Send to client %d bytes, connection %d", n, id)
		out := protocol.TransmissionUnit{protocol.Send, id, b}
		//write out result
		messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, enc *gob.Encoder, 
	ingress net.Conn, conmap map[int]*Virtcon) {
	for {
		buff, ok := <-messages
		if !ok {
			close(messages)
			return
		}

		err := enc.Encode(buff)
		if err != nil {
			if strings.Contains(err.Error(), "closed network connection") {
				log.Debugf("Error in encoder: %s", err.Error())
			} else {
				log.Errorf("Error in encoder: %s", err.Error())
			}
			ingress.Close()
		}
	}
}

func proxyConnection(ingress net.Conn, customer, postgresPort string) {
	var conmap = make(map[int]*Virtcon)
	var mu sync.RWMutex
	claim := &gotypes.Claims{}

	dec := gob.NewDecoder(ingress)
	enc := gob.NewEncoder(ingress)

	messages := make(chan protocol.TransmissionUnit, 50)
	go MultiplexWriter(messages, enc, ingress, conmap)
	

	// totalUpstream := 0

	for {
		var buff protocol.TransmissionUnit
		err := dec.Decode(&buff)

		if err != nil {
			if(err != io.EOF) {
				log.Errorf("Customer %s, read from client failed '%s', cleanup the proxy connection!", customer, err.Error())
			} else {
				log.Debugf("Customer %s, EOF!", customer)

			}
				// close all outgoing connections
			mu.Lock()
			for key := range conmap {
				conmap[key].sock.Close()
				(conmap[key]).LogUpstream(0, true)
				delete(conmap, key)
			}
			mu.Unlock()
			ingress.Close()
			return
		}
	
		switch buff.Action {
		case protocol.Open:

			jwtKey := []byte(os.Getenv("SESSION_SECRET"))
			

			_, err = jwt.ParseWithClaims(string(buff.Data), claim, func(token *jwt.Token) (interface{}, error) {
				return jwtKey, nil
			})
			if err != nil {
				log.Errorf("Error parsing token %s: %s", string(buff.Data), err.Error())
			}
			conn := &Virtcon{}
			conn.tenant = claim.Schema
			conn.email = claim.Email
			conn.session = claim.Session
			conn.groups = claim.Groups
			conn.roles = claim.Roles
			conn.accumDownstream = 0
			conn.totalDownstream = 0
			conn.accumUpstream = 0
			conn.totalUpstream = 0

			log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Connection #%d to db created, total #=%d", buff.Id, len(conmap))

			egress, err := getTargetConnection(customer, postgresPort)
			if err != nil {
				log.ErrorUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles,"Error connecting to target %s", err.Error())
			}
			conn.sock = egress
			mu.Lock()
			conmap[buff.Id] = conn
			mu.Unlock()
			go pipe(conmap, egress, messages, buff.Id, string(buff.Data), &mu)
		case protocol.Close:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if ok {
				log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Connection #%d closing, %d left", buff.Id, len(conmap) - 1)
				conn.LogUpstream(0, true)
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
					log.DebugUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Write to db error: %s", err.Error())
					conn.sock.Close()
				} else {
					l := len(buff.Data)
					bytesInLog <- l
					conn.LogUpstream(l, false)
				}
			} else {
				log.Errorf("Error finding the descriptor %d, %v", buff.Id, buff)
			}
		}

	}
}
