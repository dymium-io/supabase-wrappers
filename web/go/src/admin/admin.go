package admin

import (
	"errors"
	"fmt"
	"io"
	_ "log"
	"net/http"
	"net/url"
	"encoding/json"
	"os"
	"strings"
	"github.com/gorilla/mux"
	"dymium.com/dymium/common"
)

func AdminHandlers(p *mux.Router) {
	host := os.Getenv("ADMIN_HOST")
	b := p.Host(host).Subrouter()

	commonheaders := func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", common.Cachedirective)
		w.Header().Set("x-content-type-options", "nosniff")
		w.Header().Set("strict-transport-security", "max-age=31536000")
	}

	getImages := func(w http.ResponseWriter, r *http.Request) {

		filename := "./admin/" + r.URL.Path
		if _, err := os.Stat(filename); errors.Is(err, os.ErrNotExist) {
			// file does not exist
			w.Header().Set("Cache-Control", common.Nocache)
			w.Header().Set("Content-Type", "text/html")
			w.WriteHeader(http.StatusNotFound)

			io.WriteString(w, "<html><body>Zeitro Error 404, file not found</body></html>")
		} else {
			// file exists
			if strings.HasPrefix(r.URL.Path, "/static") || strings.HasSuffix(r.URL.Path, ".png") || strings.HasSuffix(r.URL.Path, "*.gif") ||
				strings.HasSuffix(r.URL.Path, ".jpg") || strings.HasSuffix(r.URL.Path, ".svg") {

				w.Header().Set("Cache-Control", "public, max-age=31536000, immutable")
				w.Header().Set("x-content-type-options", "nosniff")
				w.Header().Set("strict-transport-security", "max-age=31536000")
			} else {
				commonheaders(w, r)
			}
			http.ServeFile(w, r, filename)
		}
	}

	b.PathPrefix("/static").HandlerFunc(getImages)
	b.HandleFunc("/{name:.*\\.png}", getImages)
	b.HandleFunc("/{name:.*\\.gif}", getImages)
	b.HandleFunc("/{name:.*\\.svg}", getImages)
	b.HandleFunc("/{name:.*\\.jpg}", getImages)
	b.HandleFunc("/{name:.*\\.ico}", getImages)

	
	b.HandleFunc("/api/getlogin", func(w http.ResponseWriter, r *http.Request) {
		domain := os.Getenv("AUTH0_DOMAIN")
		clientid := os.Getenv("AUTH0_CLIENT_ID")
		redirecturl := os.Getenv("AUTH0_REDIRECT_URL")

		t := struct {
			LoginURL string
		}{}

		t.LoginURL = fmt.Sprintf("%sauthorize?response_type=code&client_id=%s&redirect_uri=%s",
			domain, clientid, redirecturl)

		js, err := json.Marshal(t)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")	
		w.Write(js)
	}).Methods("GET")

	b.HandleFunc("/api/getlogout", func(w http.ResponseWriter, r *http.Request) {
		domain := os.Getenv("AUTH0_DOMAIN")
		clientid := os.Getenv("AUTH0_CLIENT_ID")
		returnurl := os.Getenv("AUTH0_RETURN_URL")

		t := struct {
			LogoutURL string
		}{}

		t.LogoutURL = fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s",
			domain, url.QueryEscape(returnurl), clientid)

		js, err := json.Marshal(t)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")	
		w.Write(js)
	}).Methods("GET")

	b.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Println("in /!\n")
		http.ServeFile(w, r, "./admin/index.html")
	}).Methods("GET")
}
