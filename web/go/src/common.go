package main

import (
	_ "errors"
	_ "fmt"
	"github.com/gorilla/mux"
	"io"
	"net/http"
)

const nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
const cachedirective = "max-age=3600"

func vanillaHandlers(p *mux.Router) {

	p.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", nocache)
		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	//http.Handle("/", p)

}
