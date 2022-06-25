//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package customer

import (
	"github.com/gorilla/mux"
	"net/http"
	"os"
	"dymium.com/dymium/dhandlers"
)

func CustomerHandlers(p *mux.Router) {
	host := os.Getenv("CUSTOMER_HOST")
	nonauthenticated := p.Host(host).Subrouter()
	authenticated := nonauthenticated.Host(host).Subrouter()
	authenticated.Use(dhandlers.AuthMiddleware)

	authenticated.HandleFunc("/api/createnewconnection", dhandlers.CreateNewConnection).Methods("POST")
	authenticated.HandleFunc("/api/queryconnection", dhandlers.QueryConnection).Methods("POST")
	authenticated.HandleFunc("/api/updateconnection", dhandlers.UpdateConnection).Methods("POST")
	authenticated.HandleFunc("/api/deleteconnection", dhandlers.DeleteConnection).Methods("POST")
	authenticated.HandleFunc("/api/getconnections", dhandlers.GetConnections).Methods("GET")

	authenticated.HandleFunc("/api/savedatascope", dhandlers.SaveDatascope).Methods("POST")	
	authenticated.HandleFunc("/api/updatedatascope", dhandlers.UpdateDatascope).Methods("POST")
	authenticated.HandleFunc("/api/getdatascopedetails", dhandlers.GetDatascopeDetails).Methods("POST")
	authenticated.HandleFunc("/api/getdatascopes", dhandlers.GetDatascopes).Methods("GET")

	authenticated.HandleFunc("/api/createmapping", dhandlers.CreateMapping).Methods("POST")
	authenticated.HandleFunc("/api/updatemapping", dhandlers.UpdateMapping).Methods("POST")
	authenticated.HandleFunc("/api/deletemapping", dhandlers.DeleteMapping).Methods("POST")
	authenticated.HandleFunc("/api/getmappings", dhandlers.GetMappings).Methods("GET")

	authenticated.HandleFunc("/api/savegroups", dhandlers.SaveGroups).Methods("POST")
	authenticated.HandleFunc("/api/getgroupsfordatascopes", dhandlers.GetGroupsForDatascopes).Methods("GET")

	nonauthenticated.HandleFunc("/api/fakelogin", dhandlers.FakeLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/getlogin", dhandlers.GetLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/logout", dhandlers.GetLogout).Methods("GET")

	// For React to work properly, ensure that the URLs going into the React router return index.html
	nonauthenticated.PathPrefix("/app/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		dhandlers.Commonheaders(w, r)
		http.ServeFile(w, r, "./customer/index.html")
	})

	nonauthenticated.PathPrefix("/static").HandlerFunc(dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.png}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.gif}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.svg}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.jpg}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.ico}", dhandlers.GetImages)

	nonauthenticated.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, "./customer/index.html")
	}).Methods("GET")
}
