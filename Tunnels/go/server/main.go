package main

import (
	"dymium.com/server/tunnel"
	"encoding/json"
	"flag"
	_"fmt"
	"github.com/gorilla/mux"
	"io"
	"log"
	"net/http"
	"os"
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
		//log.Println("Healthcheck")
	}).Methods("GET")

	p.HandleFunc("/healthshellcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		w.Header().Set("Content-Type", "text/html")

		io.WriteString(w, "<html><body>OK</body></html>")
		//log.Println("Shell Healthcheck")
	}).Methods("GET")

	
	log.Println("Listen for health on :80")
	http.ListenAndServe(":80", p)
}
func main() {

	log.Print("Version 0.19")
	flag.IntVar(&port, "p", 0, "Port")
	flag.StringVar(&address, "a", "", "Address")
	flag.Parse()

	go health()

	if port == 0 {
		port = 443
	}
	log.Printf("address: %s, port %d\n", address, port)
	certificatejson := os.Getenv("CERTIFICATE")
	t := struct {
		Key         string
		Certificate string
	}{}
	err := json.Unmarshal([]byte(certificatejson), &t)
	if err != nil {
		log.Printf("Cert unmarshaling error: %s\n", err.Error())
		os.Exit(1)
	}
	passphrase := os.Getenv("PASSPHRASE")

	cajson := os.Getenv("CA_CERTIFICATE")
	tt := struct {
		Certificate string
	}{}
	
	err = json.Unmarshal([]byte(cajson), &tt)
	if err != nil {
		log.Printf("CA Cert unmarshaling error: %s\n", err.Error())
		os.Exit(1)
	}

	customer := os.Getenv("CUSTOMER")
	tunnel.Server(address, port, customer, []byte(t.Certificate), []byte(t.Key), passphrase, []byte(tt.Certificate))
}
