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


	authenticated.HandleFunc("/api/createnewconnection", dhandlers.CreateNewConnection).Methods("POST").Name("createnewconnection")
	authenticated.HandleFunc("/api/queryconnection", dhandlers.QueryConnection).Methods("POST").Name("queryconnection")
	authenticated.HandleFunc("/api/updateconnection", dhandlers.UpdateConnection).Methods("POST").Name("updateconnection")
	authenticated.HandleFunc("/api/deleteconnection", dhandlers.DeleteConnection).Methods("POST").Name("deleteconnection")
	authenticated.HandleFunc("/api/getconnections", dhandlers.GetConnections).Methods("GET").Name("getconnections")

	authenticated.HandleFunc("/api/savedatascope", dhandlers.SaveDatascope).Methods("POST").Name("savedatascope")
	authenticated.HandleFunc("/api/updatedatascope", dhandlers.UpdateDatascope).Methods("POST").Name("updatedatascope")
	authenticated.HandleFunc("/api/deletedatascope", dhandlers.DeleteDatascope).Methods("POST").Name("deletedatascope")
	authenticated.HandleFunc("/api/getdatascopedetails", dhandlers.GetDatascopeDetails).Methods("POST").Name("getdatascopedetails")
	authenticated.HandleFunc("/api/getdatascopes", dhandlers.GetDatascopes).Methods("GET").Name("getdatascopes")

	authenticated.HandleFunc("/api/createmapping", dhandlers.CreateMapping).Methods("POST").Name("createmapping")
	authenticated.HandleFunc("/api/updatemapping", dhandlers.UpdateMapping).Methods("POST").Name("updatemapping")
	authenticated.HandleFunc("/api/deletemapping", dhandlers.DeleteMapping).Methods("POST").Name("deletemapping")
	authenticated.HandleFunc("/api/getmappings", dhandlers.GetMappings).Methods("GET").Name("getmappings")

	authenticated.HandleFunc("/api/savegroups", dhandlers.SaveGroups).Methods("POST").Name("savegroups")
	authenticated.HandleFunc("/api/getgroupsfordatascopes", dhandlers.GetGroupsForDatascopes).Methods("GET").Name("getgroupsfordatascopes")
	authenticated.HandleFunc("/api/getclientcertificate", dhandlers.GetClientCertificate).Methods("POST").Name("getclientcertificate")
	authenticated.HandleFunc("/api/getdatascopesaccess", dhandlers.GetDatascopesAccess).Methods("GET").Name("getdatascopesaccess")
	authenticated.HandleFunc("/api/regenpassword", dhandlers.RegenerateDatascopePassword).Methods("GET").Name("regenpassword")


	
	nonauthenticated.HandleFunc("/api/fakelogin", dhandlers.FakeLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/getlogin", dhandlers.GetLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/logout", dhandlers.GetLogout).Methods("GET")
	nonauthenticated.HandleFunc("/api/querytunnel", dhandlers.QueryTunnel).Methods("POST")
	nonauthenticated.HandleFunc("/api/authenticatebycode", dhandlers.AuthByCode).Methods("POST")
	
	nonauthenticated.HandleFunc("/api/downloadupdate", dhandlers.DownloadUpdate).Queries("os", "{os}", "arch", "{arch}").Methods("GET")
	nonauthenticated.HandleFunc("/api/datascopehelp", dhandlers.DatascopeHelp).Queries("token", "{token}", "port", "{port}").Methods("GET")

	
	
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
	nonauthenticated.HandleFunc("/{name:.*\\.zip}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.gz}", dhandlers.GetImages)

	nonauthenticated.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, "./customer/index.html")
	}).Methods("GET")
}
