//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package customer

import (
	"github.com/gorilla/mux"
	"net/http"
	"os"
	"dymium.com/dymium/common"
	"dymium.com/dymium/dhandlers"
	_ "fmt"
)

func CustomerHandlers(p *mux.Router) {
	host := os.Getenv("CUSTOMER_HOST")
	nonauthenticated := p.Host(host).Subrouter()
	authenticated := nonauthenticated.Host(host).Subrouter()
	authenticated.Use(dhandlers.AuthMiddleware)


	authenticated.HandleFunc("/api/createnewconnection", dhandlers.CreateNewConnection).Methods("POST").Name("createnewconnection")
	authenticated.HandleFunc("/api/queryconnection", dhandlers.QueryConnection).Methods("POST").Name("queryconnection")
	authenticated.HandleFunc("/api/querytable", dhandlers.QueryTable).Methods("POST").Name("querytable")	
	authenticated.HandleFunc("/api/updateconnection", dhandlers.UpdateConnection).Methods("POST").Name("updateconnection")
	authenticated.HandleFunc("/api/deleteconnection", dhandlers.DeleteConnection).Methods("POST").Name("deleteconnection")
	authenticated.HandleFunc("/api/getconnections", dhandlers.GetConnections).Methods("GET").Name("getconnections")

	authenticated.HandleFunc("/api/savedatascope", dhandlers.SaveDatascope).Methods("POST").Name("savedatascope")
	authenticated.HandleFunc("/api/updatedatascope", dhandlers.UpdateDatascope).Methods("POST").Name("updatedatascope")
	authenticated.HandleFunc("/api/deletedatascope", dhandlers.DeleteDatascope).Methods("POST").Name("deletedatascope")
	authenticated.HandleFunc("/api/getdatascopedetails", dhandlers.GetDatascopeDetails).Methods("POST").Name("getdatascopedetails")
	authenticated.HandleFunc("/api/getdatascopes", dhandlers.GetDatascopes).Methods("GET").Name("getdatascopes")
	authenticated.HandleFunc("/api/getdatascopesfortestsql", dhandlers.GetDatascopesForTestSQL).Methods("GET").Name("getdatascopesfortestsql")

	
	authenticated.HandleFunc("/api/createmapping", dhandlers.CreateMapping).Methods("POST").Name("createmapping")
	authenticated.HandleFunc("/api/updatemapping", dhandlers.UpdateMapping).Methods("POST").Name("updatemapping")
	authenticated.HandleFunc("/api/deletemapping", dhandlers.DeleteMapping).Methods("POST").Name("deletemapping")
	authenticated.HandleFunc("/api/getmappings", dhandlers.GetMappings).Methods("GET").Name("getmappings")

	authenticated.HandleFunc("/api/savegroups", dhandlers.SaveGroups).Methods("POST").Name("savegroups")
	authenticated.HandleFunc("/api/getgroupsfordatascopes", dhandlers.GetGroupsForDatascopes).Methods("GET").Name("getgroupsfordatascopes")
	authenticated.HandleFunc("/api/getclientcertificate", dhandlers.GetClientCertificate).Methods("POST").Name("getclientcertificate")
	authenticated.HandleFunc("/api/getdatascopesaccess", dhandlers.GetDatascopesAccess).Methods("GET").Name("getdatascopesaccess")
	authenticated.HandleFunc("/api/regenpassword", dhandlers.RegenerateDatascopePassword).Methods("GET").Name("regenpassword")
	authenticated.HandleFunc("/api/getdatascopetables", dhandlers.GetDatascapeTables).Methods("POST").Name("getdatascopetables")
	authenticated.HandleFunc("/api/getselect", dhandlers.GetSelect).Methods("POST").Name("getselect")
	authenticated.HandleFunc("/api/getusage", dhandlers.GetUsage).Methods("POST").Name("getusage")
	authenticated.HandleFunc("/api/getaccesskey", dhandlers.GetAccessKeys).Methods("GET").Name("getkeyaccess")
	authenticated.HandleFunc("/api/createnewconnector", dhandlers.CreateNewConnector).Methods("POST").Name("createnewconnector")
	authenticated.HandleFunc("/api/getconnectors", dhandlers.GetConnectors).Methods("GET").Name("getconnectors")
	authenticated.HandleFunc("/api/updateconnector", dhandlers.UpdateConnector).Methods("POST").Name("updateconnector")
	authenticated.HandleFunc("/api/deleteconnector", dhandlers.DeleteConnector).Methods("POST").Name("deleteconnector")
	authenticated.HandleFunc("/api/getpolicies", dhandlers.GetPolicies).Methods("GET").Name("getpolicies")
	authenticated.HandleFunc("/api/savepolicies", dhandlers.SavePolicies).Methods("POST").Name("savepolicies")
	
	

	nonauthenticated.HandleFunc("/api/fakelogin", dhandlers.FakeLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/getlogin", dhandlers.GetLogin).Methods("GET")
	nonauthenticated.HandleFunc("/api/logout", dhandlers.GetLogout).Methods("GET")
	nonauthenticated.HandleFunc("/api/querytunnel", dhandlers.QueryTunnel).Methods("POST")
	nonauthenticated.HandleFunc("/api/authenticatebycode", dhandlers.AuthByCode).Methods("POST")
	
	nonauthenticated.HandleFunc("/api/downloadupdate", dhandlers.DownloadUpdate).Queries("os", "{os}", "arch", "{arch}").Methods("GET")
	nonauthenticated.HandleFunc("/api/downloadconnectorupdate", dhandlers.DownloadConnectorUpdate).Queries("os", "{os}", "arch", "{arch}").Methods("GET")
	

	nonauthenticated.HandleFunc("/api/datascopehelp", dhandlers.DatascopeHelp).Queries("token", "{token}", "port", "{port}").Methods("GET")
	nonauthenticated.HandleFunc("/api/getconnectorcertificate",  dhandlers.GetConnectorCertificate).Methods("POST").Name("getconnectorcertificate")

	
	
	// For React to work properly, ensure that the URLs going into the React router return index.html
	nonauthenticated.PathPrefix("/app/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		common.CommonCacheHeaders(w, r)
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
	nonauthenticated.HandleFunc("/{name:.*\\.tgz}", dhandlers.GetImages)	
	nonauthenticated.HandleFunc("/{name:.*\\.exe}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.pkg}", dhandlers.GetImages)
	nonauthenticated.HandleFunc("/{name:.*\\.json}", dhandlers.GetImages)


	nonauthenticated.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		common.CommonCacheHeaders(w, r)
		http.ServeFile(w, r, "./customer/")
	}).Methods("GET")
}
