package tunnel

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"io"
	"math/rand"
	"net"
//	"net/http"
//	_ "net/http/pprof"
	"os"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"dymium.com/dymium/log"
	"dymium.com/server/gotypes"
	"dymium.com/server/protocol"
	"github.com/golang-jwt/jwt"
	"github.com/redis/go-redis/v9"
)

var rdb *redis.Client
var ctx = context.Background()
var iprecords []net.IP
var resolvemutex sync.RWMutex
var messagesCapacity = 8
//var readBufferSize = 16 * 4096
var readBufferSize = 64

type Virtcon struct {
	sock            net.Conn
	tenant          string
	email           string
	session         string
	groups          []string
	roles           []string
	accumDownstream int64
	totalDownstream int64
	accumUpstream   int64
	totalUpstream   int64
}

func (x *Virtcon) LogDownstream(bytes int, final bool) {
	atomic.AddInt64(&x.accumDownstream, int64(bytes))
	atomic.AddInt64(&x.totalDownstream, int64(bytes))
	accumDownstream := atomic.LoadInt64(&x.accumDownstream)
	if (accumDownstream > 100000 || final) && accumDownstream > 0 {
		// sl := fmt.Sprintf("%d", x.accumDownstream)
		// log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "Downstream, sent #%d bytes", []string{sl}, x.accumDownstream)
		atomic.StoreInt64(&x.accumDownstream, 0)
	}
	if final {
		sl := fmt.Sprintf("%d", atomic.LoadInt64(&x.totalDownstream))
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "End of connection, total downstream traffic %d bytes", []string{sl}, atomic.LoadInt64(&x.totalDownstream))
	}
}
func (x *Virtcon) LogUpstream(bytes int, final bool) {
	atomic.AddInt64(&x.accumUpstream, int64(bytes))
	atomic.AddInt64(&x.totalUpstream, int64(bytes))
	accumUpstream := atomic.LoadInt64(&x.accumUpstream)
	if (accumUpstream > 100000 || final) && accumUpstream > 0 {
		sl := fmt.Sprintf("%d", x.accumUpstream)
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "Upstream, sent #%d bytes", []string{sl}, x.accumUpstream)
		atomic.StoreInt64(&x.accumUpstream, 0)
	}
	if final {
		sl := fmt.Sprintf("%d", atomic.LoadInt64(&x.totalUpstream))
		log.InfoUserArrayf(x.tenant, x.session, x.email, x.groups, x.roles, "End of connection, total upstream traffic %d bytes", []string{sl}, atomic.LoadInt64(&x.totalUpstream))
	}
}

var bytesInLog = make(chan int, 128)
var bytesOutLog = make(chan int, 128)

func logBandwidth(customer string) {
	timer := time.NewTimer(30 * time.Second)
	defer timer.Stop()

	bytesIn := 0
	bytesOut := 0

	for {
		select {
		case _bytesIn := <-bytesInLog:
			bytesIn += _bytesIn

		case _bytesOut := <-bytesOutLog:
			bytesOut += _bytesOut

		case <-timer.C:
			// Handle timer firing...
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
					log.Debugf("Error saving bytes to Redis: %s", err.Error())
				}
				bytesIn = 0
				bytesOut = 0
			}
			timer.Reset(30 * time.Second)
		}
	}
}

func initRedis(redisAddress, redisPort, redisPassword string) {
	o := &redis.Options{
		Addr: redisAddress + ":" + redisPort,
	}
	if redisPassword != "" {
		o.Password = redisPassword
	}
	rdb = redis.NewClient(o)
}
func displayBuff(what string, buff []byte) {
	if len(buff) > 100 {
		head := buff[:60]
		tail := buff[len(buff)-60:]
		log.Debugf("\n%s head: \n%s\ntail: \n%s\n", what, string(head), string(tail))
	} else {
		log.Debugf("\n%s buffer:\n%s\n", what, string(buff))
	}
}
func getTargetConnection(targetHost string, customer, postgresPort string) (net.Conn, error) {
	var err error
	resolvemutex.RLock()
	index := rand.Intn(len(iprecords))
	target := iprecords[index].String() + ":" + postgresPort
	resolvemutex.RUnlock()
	if err != nil {
		return nil, err
	}

	log.Infof("For customer %s, host: %s, target: %s", customer, targetHost, target)
	return net.Dial("tcp", target)
}
func backgroundResolve(targetHost string) {
	for {
		time.Sleep(2 * time.Minute)
		addrs, _ := net.LookupIP(targetHost)
		for _, addr := range addrs {
			log.Debugf("Resolved %s to %s", targetHost, addr.String())
		}
		resolvemutex.Lock()
		iprecords = addrs
		resolvemutex.Unlock()
	}
}
func Server(address string, port int, customer, postgressDomain, postgresPort string,
	certPEMBlock, keyPEMBlock []byte, passphrase string, caCert []byte,
	redisAddress, redisPort, redisPassword string) {
	var err error
	var pkey []byte
/*
	go func() {
		http.ListenAndServe("localhost:6060", nil)
	}()
*/
	initRedis(redisAddress, redisPort, redisPassword)

	go logBandwidth(customer)
	targetHost := customer + postgressDomain

	resolvemutex.Lock()
	iprecords, _ = net.LookupIP(targetHost)
	resolvemutex.Unlock()
	go backgroundResolve(targetHost)

	if err != nil {
		log.Errorf("Error: %s", err.Error())
		os.Exit(1)
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

	ln, err := net.Listen("tcp", connect)

	if err != nil {
		log.Errorf("Error in net.Listen: %s", err.Error())
		os.Exit(1)
	}
	defer ln.Close()
	timeoutDuration := 5 * time.Second
	for {
		conn, err := ln.Accept()
		if err != nil {
			log.Errorf("Error in Accept: %s", err.Error())
			continue
		}
	
		tcpConn, ok := conn.(*net.TCPConn)
		if !ok {
			log.Errorf("Not a TCP connection")
			conn.Close()
			continue
		}
	
		// Set TCP_NODELAY
		err = tcpConn.SetNoDelay(true)
		if err != nil {
			log.Errorf("Error setting TCP_NODELAY: %s", err.Error())
			conn.Close()
			continue
		}
	
		// Wrap with TLS
		tlsConn := tls.Server(tcpConn, config)

		go func(tlsConn *tls.Conn) {
			// perform handshake
			tlsConn.SetDeadline(time.Now().Add(timeoutDuration))
			err = tlsConn.Handshake()
			tlsConn.SetDeadline(time.Time{})
			if err != nil {
				log.Errorf("client: error during handshake, error: %s", err.Error())
				return
			}

			// get connection state and print some stuff
			state := tlsConn.ConnectionState()
			for _, cert := range state.PeerCertificates {
				for _, name := range cert.DNSNames {
					log.Debugf("Group: %s", name)
				}
			}
			
			go proxyConnection(targetHost, tlsConn, customer, postgresPort)
		}(tlsConn)
	}
}

func pipe(conmap map[int]*Virtcon, egress net.Conn, messages chan protocol.TransmissionUnit, id int, token string, mu *sync.RWMutex) {
	//arena := make([]byte, readBufferSize*(1 + messagesCapacity))
	//index := 0
	for {
		//buff := make([]byte, 4*4096)
		// buff := make([]byte, 16*4096)
		buff := make([]byte, readBufferSize)
		//buff := arena[index*readBufferSize : (index+1)*readBufferSize]
		//index = (index + 1) % (1 +  messagesCapacity)
		n, err := egress.Read(buff)
		//log.Debugf("Read from db %d bytes, connection %d", n, id)
		mu.RLock()
		conn, ok := conmap[id]
		mu.RUnlock()
		if err != nil {
			log.Errorf("Reading error %s, connection %d", err.Error(), id)
			if strings.Contains(err.Error(), "use of closed network connection") {
				// no op
			} else {
				if ok {
					if err == io.EOF {
						// be silent
					} else {
						log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles,
							"Db read failed '%s', id:%d", err.Error(), id)
					}
				} else {
					log.Errorf("Db read failed '%s', id:%d", err.Error(), id)
				}
			}
			egress.Close()
			out := protocol.TransmissionUnit{Action: protocol.Close, Id: id, Data: nil}
			messages <- out

			if ok {
				conn.LogDownstream(0, true)
			}
			return
		}
		b := buff[:n]

		//bytesOutLog <- n

		if ok {
			conn.LogDownstream(n, false)
			//log.InfoUserArrayf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Downstream, sent #%d bytes", []string{sl}, n)
		}
		log.Debugf("read %d bytes", n)
		// displayBuff("Read from db ", b)
		//log.Printf("Send to client %d bytes, connection %d", n, id)
		out := protocol.TransmissionUnit{Action: protocol.Send, Id: id, Data: b}
		//write out result
		messages <- out
	}
}

func MultiplexWriter(messages chan protocol.TransmissionUnit, 
	ingress net.Conn, conmap map[int]*Virtcon) {
	for {
		buff, ok := <-messages
		if !ok {
			close(messages)
			return
		}
		log.Debugf("In Write: Action: %d,\n%s\n", buff.Action, string(buff.Data) )
		err := protocol.WriteToTunnel(&buff, ingress)
		if err != nil {
			if strings.Contains(err.Error(), "closed network connection") {
				log.Debugf("Error in writer: %s", err.Error())
			} else {
				log.Errorf("Error in writer: %s", err.Error())
			}
			ingress.Close()
		} else {
			//log.Debugf("Encoded  %d bytes", len(buff.Data) )
		}
	}
}


func proxyConnection(targetHost string, ingress net.Conn, customer, postgresPort string) {
	var conmap = make(map[int]*Virtcon)
	var mu sync.RWMutex
	claim := &gotypes.Claims{}


	messages := make(chan protocol.TransmissionUnit, messagesCapacity)
	go MultiplexWriter(messages, ingress, conmap)

	// totalUpstream := 0
	st := make([]byte, protocol.ProtocolChunkSize)
	for {
		var buff protocol.TransmissionUnit
		_,  err := protocol.ReadFull(ingress, st, 9)

		protocol.GetTransmissionUnit(st, &buff, ingress)
		

		if err != nil {
			if err != io.EOF {
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

			var p jwt.Parser
			_, _, err = p.ParseUnverified(string(buff.Data), claim) // only informational
			if err != nil {
				log.Errorf("Error: token invalid, can't continue %s", err.Error())
				os.Exit(1)
			}
			/*
				jwtKey := []byte(os.Getenv("SESSION_SECRET"))
				_, err = jwt.ParseWithClaims(string(buff.Data), claim, func(token *jwt.Token) (interface{}, error) {
					return jwtKey, nil
				})
				if err != nil {
					log.Errorf("Error parsing token %s: %s", string(buff.Data), err.Error())
				}
			*/
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
			mu.RLock()
			howmany := len(conmap)
			mu.RUnlock()
			log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Connection #%d to db creating, total existing #=%d", buff.Id, howmany)

			egress, err := getTargetConnection(targetHost, customer, postgresPort)
			if err != nil {
				log.ErrorUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Error connecting to target %s", err.Error())
				messages <- protocol.TransmissionUnit{Action: protocol.Close, Id: buff.Id, Data: nil}
				return
			}
			conn.sock = egress
			mu.Lock()
			conmap[buff.Id] = conn
			howmany = len(conmap)
			mu.Unlock()
			log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Connection #%d to db created, total #=%d", buff.Id, howmany)

			go pipe(conmap, egress, messages, buff.Id, string(buff.Data), &mu)
		case protocol.Close:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			howmany := len(conmap) - 1
			mu.RUnlock()
			if ok && conn != nil {
				log.InfoUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Connection #%d closing, %d left", buff.Id, howmany)
				conn.LogUpstream(0, true)
				if conn != nil && conn.sock != nil {
					conn.sock.Close()
				}
				mu.Lock()
				delete(conmap, buff.Id)
				mu.Unlock()
			} else {
				log.Errorf("Closing connection: error finding the descriptor %d, %v", buff.Id, buff)
			}
		case protocol.Send:
			mu.RLock()
			conn, ok := conmap[buff.Id]
			mu.RUnlock()
			if ok && conn != nil {
				if conn.sock != nil {
					displayBuff("write:", buff.Data)
					n, err := conn.sock.Write(buff.Data)
					if err != nil {
						log.DebugUserf(conn.tenant, conn.session, conn.email, conn.groups, conn.roles, "Write to db error: %s", err.Error())
						conn.sock.Close()
					} else {
						//log.Debugf("Wrote  %d bytes, connection %d", n, buff.Id)
						//bytesOutLog <- n
						conn.LogUpstream(n, false)
					}
				} else {
					log.Errorf("Error: Connection %d already cleared", buff.Id)
				}
			} else {
				log.Errorf("Error finding the descriptor %d, %v", buff.Id, buff)
			}
		}

	}
}
