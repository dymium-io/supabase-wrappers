package main

import (
	"encoding/json"
	_ "fmt"
	"io"
	"net/http"
	"os"
	"strconv"

	"dymium.com/dymium/log"
	"dymium.com/meshserver/tunnel"
	"github.com/gorilla/mux"
)

const Nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"

var port int
var address string

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
	log.Init("Mesh Tunnel")
	log.Info("Starting Mesh server, Version 0.01")

	if "true" != os.Getenv("LOCAL_ENVIRONMENT") {
		go health()
	}
	sport := os.Getenv("PORT")
	if sport != "" {
		port, _ = strconv.Atoi(sport)
	}
	if port == 0 {
		port = 443
	}
	log.Debugf("port %d", port)

	address := os.Getenv("ADDRESS")

	certificatejson := os.Getenv("CERTIFICATE")
	cert := struct {
		Key         string
		Certificate string
	}{}
	err := json.Unmarshal([]byte(certificatejson), &cert)
	if err != nil {
		log.Errorf("Cert unmarshaling error: %s", err.Error())
		os.Exit(1)
	}

	passphrase := os.Getenv("PASSPHRASE")
	cajson := os.Getenv("CA_CERTIFICATE")
	ca := struct {
		Certificate string
	}{}

	err = json.Unmarshal([]byte(cajson), &ca)
	if err != nil {
		log.Errorf("CA Cert unmarshaling error: %s", err.Error())
		os.Exit(1)
	}

	customer := os.Getenv("CUSTOMER")
	dbPort := os.Getenv("DATABASE_PORT")
	dbDomain := os.Getenv("DATABASE_HOST")
	dbUsername := os.Getenv("DATABASE_USER")
	dbPassword := os.Getenv("DATABASE_PASSWORD")
	dbName := os.Getenv("DATABASE_NAME")
	tls := os.Getenv("DATABASE_TLS")

	tunnel.Server(address, port, customer, []byte(cert.Certificate),
		[]byte(cert.Key), passphrase, []byte(ca.Certificate), dbDomain, dbPort, dbUsername, dbPassword, dbName, tls)
}
