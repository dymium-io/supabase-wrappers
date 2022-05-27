package common

import (
	_ "errors"
	_ "fmt"
	"github.com/gorilla/mux"
	"io"
	"net/http"
	"strings"
)

const Nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
const Cachedirective = "max-age=3600"

func VanillaHandlers(p *mux.Router) {

	p.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	//http.Handle("/", p)

}

// this function extracts JWT from the request
func TokenFromHTTPRequest(r *http.Request) string {
	reqToken := r.Header.Get("Authorization")
	var tokenString string
	splitToken := strings.Split(reqToken, "Bearer ")
	if len(splitToken) > 1 {
		tokenString = splitToken[1]
	}
	return tokenString
}