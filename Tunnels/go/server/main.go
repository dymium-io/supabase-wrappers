package main
import (
	"fmt"
	"flag"
	"os"
	"log"
	"io"
	"net/http"
	"encoding/json"
	"dymium.com/server/tunnel"
	"github.com/gorilla/mux"
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

	http.ListenAndServe("localhost:80", p)
}
func main() {

	flag.IntVar( &port, "p", 0, "Port")
	flag.StringVar(&address, "a", "", "Address")
	flag.Parse()
	fmt.Printf("Args %v\n", os.Args)

	go health()

	if len(address) == 0 {
		address = "localhost"
	}
    if port == 0  {
		port = 443
    }
	certificatejson := os.Getenv("CERTIFICATE")
	t := struct {
		Key string
		Certificate string
	}{}	
	err := json.Unmarshal([]byte(certificatejson), &t)
	if err != nil {
		log.Printf("Cert unmarshaling error: %s\n", err.Error() )
		os.Exit(1)
	}
	passphrase := os.Getenv("PASSPHRASE")

	cajson := os.Getenv("CA_CERTIFICATE")
	tt := struct {
		Certificate string
	}{}	
	fmt.Printf("%s\n", cajson)
	err = json.Unmarshal([]byte(cajson), &tt)
	if err != nil {
		log.Printf("CA Cert unmarshaling error: %s\n", err.Error() )
		os.Exit(1)
	}

	customer := os.Getenv("CUSTOMER")
	tunnel.Server(address, port, customer, []byte(t.Certificate), []byte(t.Key), passphrase, []byte(tt.Certificate))
}