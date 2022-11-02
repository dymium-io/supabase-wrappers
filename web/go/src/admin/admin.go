// 
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package admin

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	_"encoding/json"
	"os"
	"strings"
	"github.com/gorilla/mux"
	"dymium.com/dymium/common"
	_ "dymium.com/dymium/log"
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

			io.WriteString(w, "<html><body>Dymium Error 404, file not found</body></html>")
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


	b.HandleFunc("/api/logout", func(w http.ResponseWriter, r *http.Request) {
		domain := os.Getenv("AUTH0_ADMIN_DOMAIN")
		clientid := os.Getenv("AUTH0_ADMIN_CLIENT_ID")
		returnurl := os.Getenv("AUTH0_ADMIN_RETURN_URL")

		logoutURL := fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s",
			domain, url.QueryEscape(returnurl), clientid)

		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")	
		http.Redirect(w, r, logoutURL, 302)

	}).Methods("GET")


	// For React to work properly, ensure that the URLs going into the React router return index.html
	b.PathPrefix("/app/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})
	b.PathPrefix("/services/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})
	b.PathPrefix("/resources/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})

	b.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Println("in /!")
		http.ServeFile(w, r, "./admin/index.html")
	}).Methods("GET")
}
