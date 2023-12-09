package main

import (
	"dymium.com/server/tunnel"
	"encoding/json"
	"flag"
	_"fmt"
	"github.com/gorilla/mux"
	"io"
	"net/http"
	"os"
	"dymium.com/dymium/log"
)

const Nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"

var address string
var port int

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

	
	log.Infof("Listen for health on :80")
	http.ListenAndServe(":80", p)
}
func main() {
	log.Init("tunnel")
	log.Info("Starting tunnel server, Version 0.20")
	flag.IntVar(&port, "p", 0, "Port")
	flag.StringVar(&address, "a", "", "Address")
	flag.Parse()

	go health()

	if port == 0 {
		port = 443
	}
	loglevel := os.Getenv("LOG_LEVEL")
	log.Infof("Log level: %s", loglevel)
	log.Debugf("address: %s, port %d", address, port)
	certificatejson := os.Getenv("CERTIFICATE")
	t := struct {
		Key         string
		Certificate string
	}{}
	err := json.Unmarshal([]byte(certificatejson), &t)
	if err != nil {
		log.Errorf("Cert unmarshaling error: %s", err.Error())
		os.Exit(1)
	}
	passphrase := os.Getenv("PASSPHRASE")

	cajson := os.Getenv("CA_CERTIFICATE")
	tt := struct {
		Certificate string
	}{}
	
	err = json.Unmarshal([]byte(cajson), &tt)
	if err != nil {
		log.Errorf("CA Cert unmarshaling error: %s", err.Error())
		os.Exit(1)
	}

	customer := os.Getenv("CUSTOMER")
	postgresPort := os.Getenv("POSTGRES_PORT")
	postgressDomain := os.Getenv("POSTGRES_DOMAIN")
	if postgresPort == "" {
		postgresPort = "5432"
	}
	if postgressDomain == "" {
		postgressDomain = ".guardian.local"
	}

	log.Infof("proxy domain: %s, port %s", postgressDomain, postgresPort)

	redisPort := os.Getenv("REDIS_PORT")
	redisbAddress := os.Getenv("REDIS_HOST")
	redisPassword := os.Getenv("REDIS_PASSWORD")
	
	tunnel.Server(address, port, customer, postgressDomain, postgresPort, []byte(t.Certificate), 
		[]byte(t.Key), passphrase, []byte(tt.Certificate), redisbAddress, redisPort, redisPassword)
}
