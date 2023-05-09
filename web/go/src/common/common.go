//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package common

import (
	_ "errors"
	_ "fmt"
	"io"
	"net/http"
	"os"
	"strings"

	"github.com/gorilla/mux"
)

const Nocache = "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
const Cachedirective = "public, max-age=3600, immutable"

var customerhost = os.Getenv("CUSTOMER_HOST")
var adminhost = os.Getenv("ADMIN_HOST")
var csp = "default-src 'self' https://cdn.jsdelivr.net https://docs.google.com/; frame-ancestors 'self'; form-action 'self'; script-src 'self';  img-src * data: ; style-src * 'a256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=' 'sha256-o552xDUU8SryzTHrTVY6r/0KlybFQD5idiatQBdm3Ds=' 'sha256-vxkJeEt/Zj9tmU6kmft+mY7UO1KIdI1vsptJ3lW43rs=';"

func VanillaHandlers(p *mux.Router) {

	p.HandleFunc("/healthcheck", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", Nocache)
		io.WriteString(w, "<html><body>OK</body></html>")
	}).Methods("GET")

	//http.Handle("/", p)

}
func CommonCacheHeaders(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Cache-Control", Cachedirective)
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
	w.Header().Set("Content-Security-Policy", csp)
	w.Header().Set("X-Frame-Options", "sameorigin")
}
func CommonNocacheHeaders(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Cache-Control", Nocache)
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
	w.Header().Set("Content-Security-Policy", csp)
	w.Header().Set("X-Frame-Options", "sameorigin")
}
func CommonNocacheNocspHeaders(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Cache-Control", Nocache)
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
	w.Header().Set("X-Frame-Options", "sameorigin")
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