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
	"dymium.com/dymium/ahandlers"
)

func AdminHandlers(p *mux.Router) {
	host := os.Getenv("ADMIN_HOST")
	nonauthenticated := p.Host(host).Subrouter()
	authenticated := nonauthenticated.Host(host).Subrouter()
	authenticated.Use(ahandlers.AuthMiddleware)


	authenticated.HandleFunc("/api/createnewcustomer", ahandlers.CreateNewCustomer).Methods("POST").Name("createnewcustomer")
	authenticated.HandleFunc("/api/getcustomers", ahandlers.GetCustomers).Methods("GET").Name("getcustomers")
	authenticated.HandleFunc("/api/deletecustomer", ahandlers.DeleteCustomer).Methods("POST").Name("deletecustomer")
	authenticated.HandleFunc("/api/updatecustomer", ahandlers.UpdateCustomer).Methods("POST").Name("updatecustomer")
	authenticated.HandleFunc("/api/getglobalusage", ahandlers.GetGlobalUsage).Methods("GET").Name("getglobalusage")

	
	getImages := func(w http.ResponseWriter, r *http.Request) {

		filename := "./admin/" + r.URL.Path
		if _, err := os.Stat(filename); errors.Is(err, os.ErrNotExist) {
			// file does not exist
			common.CommonNocacheHeaders(w, r)
			w.Header().Set("Content-Type", "text/html")
			w.WriteHeader(http.StatusNotFound)

			io.WriteString(w, "<html><body>Dymium Error 404, file not found</body></html>")
		} else {
			// file exists
			if strings.HasPrefix(r.URL.Path, "/static") || strings.HasSuffix(r.URL.Path, ".png") || strings.HasSuffix(r.URL.Path, "*.gif") ||
				strings.HasSuffix(r.URL.Path, ".jpg") || strings.HasSuffix(r.URL.Path, ".svg") {

				common.CommonCacheHeaders(w, r)
			} else {
				common.CommonNocacheHeaders(w, r)
			}
			http.ServeFile(w, r, filename)
		}
	}

	nonauthenticated.PathPrefix("/static").HandlerFunc(getImages)
	nonauthenticated.HandleFunc("/{name:.*\\.png}", getImages)
	nonauthenticated.HandleFunc("/{name:.*\\.gif}", getImages)
	nonauthenticated.HandleFunc("/{name:.*\\.svg}", getImages)
	nonauthenticated.HandleFunc("/{name:.*\\.jpg}", getImages)
	nonauthenticated.HandleFunc("/{name:.*\\.ico}", getImages)


	nonauthenticated.HandleFunc("/api/logout", func(w http.ResponseWriter, r *http.Request) {
		domain := os.Getenv("AUTH0_ADMIN_DOMAIN")
		clientid := os.Getenv("AUTH0_ADMIN_CLIENT_ID")
		returnurl := os.Getenv("AUTH0_ADMIN_RETURN_URL")

		logoutURL := fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s",
			domain, url.QueryEscape(returnurl), clientid)

		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "text/html")	
		http.Redirect(w, r, logoutURL, 302)

	}).Methods("GET")


	// For React to work properly, ensure that the URLs going into the React router return index.html
	nonauthenticated.PathPrefix("/app/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		common.CommonCacheHeaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})
	nonauthenticated.PathPrefix("/services/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		common.CommonCacheHeaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})
	nonauthenticated.PathPrefix("/resources/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		common.CommonCacheHeaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	})

	nonauthenticated.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Println("in /!")
		common.CommonCacheHeaders(w, r)
		http.ServeFile(w, r, "./admin/index.html")
	}).Methods("GET")
}
