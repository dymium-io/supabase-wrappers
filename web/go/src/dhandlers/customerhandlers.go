//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package dhandlers

import (
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	"dymium.com/dymium/log"
	_"github.com/gorilla/mux"
	"golang.org/x/net/context"
	"github.com/Jeffail/gabs"	
	"github.com/gorilla/mux"
	"encoding/json"
	"io/ioutil"
	"net/url"
	"crypto/rand"
	"math/big"
	"time"
	"errors"
	"crypto/x509"
	"encoding/pem"	
	"io"
	"net/http"
	"strconv"
	"os"
	"strings"
	"path"
	"fmt"
)
type contextKey int
const authenticatedSchemaKey contextKey = 0
const authenticatedEmailKey contextKey = 1
const authenticatedGroupsKey contextKey = 2
const authenticatedOrgKey contextKey = 3
const authenticatedRolesKey contextKey = 4
const authenticatedSessionKey contextKey = 5

func AuthMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
		token := common.TokenFromHTTPRequest(r)
		schema, roles, groups, email, orgid, session, error := authentication.GetSchemaRolesFromToken(token)

		if error != nil {
			log.Errorf("Auth Error: %s", error.Error())
			status := types.OperationStatus{"AuthError", error.Error()}
			js, err := json.Marshal(status)
		
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			w.Header().Set("Cache-Control", common.Nocache)
			w.Header().Set("Content-Type", "text/html")
			w.Write(js)
			return
		}
		//create a new request context containing the authenticated user
		ctxWithSchema := context.WithValue(r.Context(), authenticatedSchemaKey, schema)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedGroupsKey, groups)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedEmailKey, email)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedOrgKey, orgid)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedRolesKey, roles)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedSessionKey, session)
		//create a new request using that new context
		rWithSchema := r.WithContext(ctxWithSchema)

		use := authentication.Authorized(r, roles)
		if !use {

			status := types.OperationStatus{"AuthError", "Role is not authorized for this resource"}
			js, err := json.Marshal(status)
		
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			w.Header().Set("Cache-Control", common.Nocache)
			w.Header().Set("Content-Type", "text/html")
			w.Write(js)
			return			
		}
		h.ServeHTTP(w, rWithSchema)
	})
}

func Commonheaders(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Cache-Control", common.Cachedirective)
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
}
func QueryConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	sessions := r.Context().Value(authenticatedSessionKey).(string)

	status := types.ConnectionDetailResponse{"OK", "", nil}
	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.ConnectionDetailRequest{}
	err := json.Unmarshal(body, &t)

	// get the connection details
	conn, err := authentication.GetConnection(schema, t.ConnectionId)
	if(err != nil) {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api Error: %s", err.Error())
	} else {
		log.InfoUserf(schema, sessions, email, groups, roles, "Api QueryConnection %s, success", conn.Database)
	}
	bconn, err := json.Marshal(conn)

	invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api DbAnalyzer Error: %s", err.Error())
		status =  types.ConnectionDetailResponse{"Error", err.Error(), nil}
		
	} else {
		jsonParsed, err := gabs.ParseJSON(invokebody)
		if(err != nil) {
			log.ErrorUserf(schema, sessions, email, groups, roles, "Api DbAnalyzer Error parsing output: %s", err.Error())
		}
		value, ok := jsonParsed.Path("Errormessage").Data().(string)
		if(ok) {
			rr := fmt.Sprintf("Api Error in DbAnalyzer Invoke return: %s", value)
			status = types.ConnectionDetailResponse{"Error", rr, nil}
		
		} else {
			t := types.Database{}
			err := json.Unmarshal(invokebody, &t)
			if(err != nil) {
				status =  types.ConnectionDetailResponse{"Error", err.Error(), nil}
			} else {
				status =  types.ConnectionDetailResponse{"OK", "", &t}
			}
		}
	}
	
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func SaveDatascope(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.Datascope{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveDatascope, error %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.SaveDatascope(schema, t)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Ghost Database created"}
		log.InfoUserf(schema, session, email, groups, roles, "Api SaveDatascope(%s), success", t.Name )

	} else {
		status = types.OperationStatus{"Error", error.Error()}
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveDatascope, error %s", error.Error() )

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)			
		return	
	}

	var rq types.Request
	rq.Action = types.A_Update
	rq.Customer = schema
	rq.Datascope = &t.Name
	snc, _ := json.Marshal(rq)

	invokebody, err := authentication.Invoke("DbSync", nil, snc)
	if err != nil {
		log.Errorf("Api SaveDatascope, DbSync Invoke Error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")		
		w.Write(js)		
		return
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.Errorf("Api SaveDatascope, DbSync Error parsing output: %s", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if(ok) {
		rr := fmt.Sprintf("Api SaveDatascope, Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.Errorf("Api %s", rr)
	} 

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)		
}

func GetUsage(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	_, _ = ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	bytesin, bytesout,logins,tunnels, err := authentication.GetBytes(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	var t types.Usage

	t.Bytesin = strconv.FormatInt(bytesin, 10) 
	t.Bytesout = strconv.FormatInt(bytesout, 10) 
	t.Logins = logins //
	t.Tunnels = tunnels 

	connections, datascopes, blocked, obfuscated, redacted, connectors, ctunnels, err := authentication.GetRestrictions(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, error: %s", err.Error())

		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	t.Connections = connections
	t.Datascopes = datascopes
	t.Blocked = blocked
	t.Obfuscated = obfuscated 
	t.Redacted = redacted
	t.Connectors = connectors
	t.Connectortunnels = ctunnels

	js, err := json.Marshal(t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetUsage, success" )
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)	
}

func GetSelect(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeTable

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	ds, err := authentication.GetSelect(schema, &t)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	js, err := json.Marshal(ds)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	log.InfoUserf(schema, session, email, groups, roles, "Api GetSelect, success" )

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)

}

func GetDatascapeTables(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeId

	err := json.Unmarshal(body, &t)

	var status types.DatascopeTables
	ds, err := authentication.GetDatascopeTables(schema, t.Id)

	if(err != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascapeTables, error: %s", err.Error())
		status = types.DatascopeTables{"Error", err.Error(), nil} 
	} else {
		status = types.DatascopeTables{"OK", "", ds} 
	}
	js, err := json.Marshal(status)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascapeTables, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	var outs []string
	for i:=0; i < len(ds); i++  {
		outs = append(outs, ds[i].Table)
	}
	log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetDatascapeTables, success", outs)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}

func GetDatascopeDetails(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeId

	err := json.Unmarshal(body, &t)

	var status types.DatascopeInfoStatus
	ds, err := authentication.GetDatascope(schema, t.Id)
	if(err != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascopeDetails %s, Error: %s", ds.Name, err.Error())
		status = types.DatascopeInfoStatus{"Error", err.Error(), nil} 
	} else {
		log.InfoUserf(schema, session, email, groups, roles, "Api GetDatascopeDetails %s, success", ds.Name)
		status = types.DatascopeInfoStatus{"OK", "", &ds} 
	}
	js, err := json.Marshal(status)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascopeDetails %s, Error: %s", ds.Name, err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func UpdateConnection(w http.ResponseWriter, r *http.Request) {
	var status types.OperationStatus

	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.ConnectionRecord
	err := json.Unmarshal(body, &t)


	if err == nil {		
		conn, err := authentication.GetConnection(schema, *t.Id)
		if(err != nil) {
			log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, error %s, Id %s", err.Error(), *t.Id )
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		conn.TestOnly = true
		conn.Typ = types.ConnectionType(t.Dbtype)
		conn.Address = t.Address
		conn.Port = t.Port
		conn.Database = t.Dbname
		conn.Tls = t.UseTLS 
		conn.TestOnly = true
		if(t.Username != nil && t.Password != nil) {
			conn.User = *t.Username 
			conn.Password = *t.Password
		}
		bconn, err := json.Marshal(conn)

		invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
		if err != nil {
			log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, DbAnalyzer error: %s", err.Error())
			status = types.OperationStatus{"Error", err.Error()}
			
		} else {
			jsonParsed, err := gabs.ParseJSON(invokebody)
			if(err != nil) {
				log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, DbAnalyzer error parsing output: %s", err.Error())
			}
			value, ok := jsonParsed.Path("Errormessage").Data().(string)
			if(ok) {
				rr := fmt.Sprintf("UpdateConnection, Error in Invoke return: %s", value)
				status = types.OperationStatus{"Error", rr}
				log.ErrorUserf(schema, session, email, groups, roles, "Api %s", rr)
			}
		}
		if(status.Status == "Error") {
			js, err := json.Marshal(status) //
			if err != nil {
				log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, Error: %s", err.Error())
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			w.Header().Set("Cache-Control", common.Nocache)
			w.Header().Set("Content-Type", "text/html")
			w.Write(js)
			return
		}
	}

	error := authentication.UpdateConnection(schema, t)

	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, error: %s", error.Error())
		status = types.OperationStatus{"Error", error.Error()}
		http.Error(w, error.Error(), http.StatusInternalServerError)
	} else {
		log.InfoUserf(schema, session, email, groups, roles, "Api UpdateConnection, success")
		status = types.OperationStatus{"OK", "Connection updated"}
	}
	js, err := json.Marshal(status) //
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func DeleteMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t struct {
		Id string
	}
	err := json.Unmarshal(body, &t)

	error := authentication.DeleteMapping(schema, t.Id)

	var status types.OperationStatus
	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteMapping, error: %s", error.Error())
		status = types.OperationStatus{"Error", error.Error()}
	} else {
		status = types.OperationStatus{"OK", "Mapping deleted"}
	}
	js, err := json.Marshal(status) //
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteMapping, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if(error == nil) {
		log.InfoUserf(schema, session, email, groups, roles, "Api DeleteMapping, success")
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func UpdateMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupMapping
	err := json.Unmarshal(body, &t)

	error := authentication.UpdateMapping(schema, *t.Id, t.Dymiumgroup, t.Directorygroup, t.Comments, t.Adminaccess)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Mapping updated"}
	} else {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateMapping, error: %s", error.Error())
		status = types.OperationStatus{"Error", error.Error()}
	}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateMapping, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if(error == nil) {
		log.InfoUserf(schema, session, email, groups, roles, "Api UpdateMapping, success")
	}	
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func CreateMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupMapping
	err := json.Unmarshal(body, &t)

	error := authentication.CreateNewMapping(schema, t.Dymiumgroup, t.Directorygroup, t.Comments)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Mapping created"}
	} else {
		log.ErrorUserf(schema, session, email, groups, roles, "Api CreateMapping, error: %s", error.Error())
		status = types.OperationStatus{"Error", error.Error()}
	}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api CreateMapping, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if(error == nil) {
		log.InfoUserf(schema, session, email, groups, roles, "Api CreateMapping %s to %s, success", t.Directorygroup, t.Dymiumgroup)
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func CreateNewConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.ConnectionRecord
	err := json.Unmarshal(body, &t)

	id, error := authentication.CreateNewConnection(schema,  t)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Connection created"}
	} else {
		status = types.OperationStatus{"Error", error.Error()}
		log.ErrorUserf(schema, session, email, groups, roles, "Error: %s", error.Error())
	}

	if(error == nil) {
		// get the connection details
		conn, err := authentication.GetConnection(schema, id)

		if err == nil {
			log.InfoUserf(schema, session, email, groups, roles, "Api Connection to %s created", conn.Database)
			conn.TestOnly = true
			bconn, err := json.Marshal(conn)
			log.Infof("conn: %s", string(bconn))
			invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
			if err != nil {
				log.ErrorUserf(schema, session, email, groups, roles, "Api DbAnalyzer Error: %s", err.Error())
				status = types.OperationStatus{"Error", err.Error()}
			} else {
				jsonParsed, err := gabs.ParseJSON(invokebody)
				if(err != nil) {
					log.ErrorUserf(schema, session, email, groups, roles, "Api DbAnalyzer Error parsing output: %s", err.Error())
				}
				value, ok := jsonParsed.Path("Errormessage").Data().(string)
				if(ok) {
					rr := fmt.Sprintf("Error in Invoke return: %s", value)
					status = types.OperationStatus{"Error", rr}
					log.ErrorUserf(schema, session, email, groups, roles, "Api %s", rr)
				} 
			}
		} else {
			log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetConnection to %s: %s", conn.Database, err.Error())
		}
	}
	if(status.Status == "Error") {
		authentication.DeleteConnection(schema, id)
		log.ErrorUserf(schema, session, email, groups, roles, "Api Roll back connection creation")
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func UpdateDatascope(w http.ResponseWriter, r *http.Request) {
	var status types.OperationStatus
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.Datascope{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, unmarshaling error: %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.UpdateDatascope(schema, t)
	if error != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope Error: %s", error.Error())
		status = types.OperationStatus{"Error", error.Error()}
		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")		
		w.Write(js)		
		return		
	}

	var rq types.Request
	rq.Action = types.A_Update
	if( len(t.Records) == 0) {
		rq.Action = types.A_Delete
	}

	rq.Customer = schema
	rq.Datascope = &t.Name
	snc, _ := json.Marshal(rq)

	invokebody, err := authentication.Invoke("DbSync", nil, snc)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, DbSync Invoke Error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")		
		w.Write(js)		
		return
	} 

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, DbSync Error parsing output: %s", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if(ok) {
		rr := fmt.Sprintf("Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, %s", rr)
	} else {
		status = types.OperationStatus{"OK", ""}
		log.InfoUserf(schema, session, email, groups, roles, "Api UpdateDatascope %s, success", t.Name)
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)		
}

func DeleteDatascope(w http.ResponseWriter, r *http.Request) {
	var status types.OperationStatus
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.DatascopeIdName{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, unmarshaling error: %s", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	var rq types.Request
	rq.Action = types.A_Delete

	rq.Customer = schema
	rq.Datascope = &t.Name
	snc, _ := json.Marshal(rq)

	invokebody, err := authentication.Invoke("DbSync", nil, snc)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, DbSync Invoke Error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")		
		w.Write(js)		
		return
	} 

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, DbSync Error parsing output: %s", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	var js []byte
	if(ok) {
		rr := fmt.Sprintf("Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, %s", rr)
	} else {
		status = types.OperationStatus{"OK", ""}
		error := authentication.DeleteDatascope(schema, t.Id)
		if error != nil {
			log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, Error: %s", error.Error())
			status = types.OperationStatus{"Error", error.Error()}
		}
	}


	js, err = json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api DeleteDatascope(%s), success", t.Name)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)		
}

func SaveGroups(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupAssignment

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveGroups, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.UpdateGroupAssignment(schema, t)

	var status  types.OperationStatus
	if error == nil {
		status = types.OperationStatus{"OK", "Groups Updated"}
	} else {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveGroups, error: %s", error.Error())
		status = types.OperationStatus{"Error", err.Error()}
	}
	
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveGroups, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var out []string
	for i := 0; i < len(t.Groups); i++ {
		out = append(out, t.Groups[i].Name)
	}
	if(error == nil) {
		log.InfoUserArrayf(schema, session, email, groups, roles, "Api SaveGroups for Datascope %s, success", out, t.Name)
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func DeleteConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := struct {
		Id string
	}{}

	err := json.Unmarshal(body, &t)
	
	conn, err := authentication.GetConnection(schema, t.Id)
	if(err != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in DeleteConnection: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)		
	}
	err = authentication.DeleteConnection(schema, t.Id)
	var status  types.OperationStatus
	if err == nil {
		status = types.OperationStatus{"OK", "Connection deleted"}
		log.InfoUserf(schema, session, email, groups, roles, "Api DeleteConnection %s success", conn.Database)

	} else {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteConnection, error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}
	}
	
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}

func SavePolicies(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	err := authentication.SavePolicies(schema, body)

	var status  types.OperationStatus
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SavePolicies, error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}
	} else {
		log.InfoUserf(schema, session, email, groups, roles, "Api SavePolicies success")
		status = types.OperationStatus{"OK", "Policies Saved"}
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
		
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)

}

func GetPolicies(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	
	js, err := authentication.GetPolicies(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetPolicies, error: %s", err.Error())

		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	} else {
		log.InfoUserf(schema, session, email, groups, roles, "Api GetPolicies success")

	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	datascopes, error := authentication.GetDatascopes(schema)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetDatascopes: %s", error.Error())

		status := types.DatascopesStatus{"Error", error.Error(), []types.DatascopeIdName{}}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	status := types.DatascopesStatus{"OK", "", datascopes}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetDatascopes: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	} 
	var out []string 
	for i := 0; i < len(datascopes); i++ {
		out = append(out, datascopes[i].Name)
	}
	log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetDatascopes, success", out)
	w.Write(js)
}
func GetMappings(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	mappings, error := authentication.GetMappings(schema)
	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetMappings, error: %s", error.Error())

		var status  types.GroupMappingStatus
		status = types.GroupMappingStatus{"Error", error.Error(), []types.GroupMapping{}}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	status := types.GroupMappingStatus{"OK", "", mappings}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetMappings, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var out []string
	for i := 0; i < len(mappings); i++ {
		s := fmt.Sprintf("%s to %s, ",  mappings[i].Directorygroup, mappings[i].Dymiumgroup)
		out = append(out, s)
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetMappings, success")
	w.Write(js)
}

func GetGroupsForDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	
	mappings, error := authentication.GetGroupAssignments(schema)
	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetGroupsForDatascopes, error: %s", error.Error())
		var status  types.OperationStatus
		status = types.OperationStatus{"Error", error.Error()}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	js, err := json.Marshal(mappings)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetGroupsForDatascopes, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var out []string
	for i := 0; i < len(mappings); i++ {
		s := fmt.Sprintf("%s available to %s, ",  mappings[i].Name, mappings[i].Groupname)
		out = append(out, s)
	}

	log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetGroupsForDatascopes, success", out)
	w.Write(js)
}
func FakeLogin(w http.ResponseWriter, r *http.Request) {

	if(r.Host != "portal.dymium.local" || r.Host != os.Getenv("CUSTOMER_HOST")) {
		log.Errorf("Error: FakeLogin on non-local host requested")
		http.Error(w, "Fake login forbidden", http.StatusInternalServerError)
		return
	}
	js := authentication.GetFakeAuthentication()
	log.Infof("FakeLogin on local host,  success")
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func GetLogin(w http.ResponseWriter, r *http.Request) {
	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	redirecturl := os.Getenv("AUTH0_PORTAL_REDIRECT_URL")

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
	log.Infof("Api GetLogin, success")
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}

func GetLogout(w http.ResponseWriter, r *http.Request) {
	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	returnurl := os.Getenv("AUTH0_PORTAL_RETURN_URL")

	logoutURL := fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s&federated",
		domain, url.QueryEscape(returnurl), clientid)
	//

	token := common.TokenFromHTTPRequest(r)
	if token != "" {
		schema, roles, groups, email, _, session, err := authentication.GetSchemaRolesFromToken(token)
		if err == nil {
			log.InfoUserf(schema, session, email, groups, roles, "Api GetLogout, success")
		} else {
			log.Infof("Api GetLogout, session expired")			
		}

	} else {
		log.Infof("Api GetLogout, success")

	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	http.Redirect(w, r, logoutURL, 302)

}

func AuthByCode(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.AuthorizationCodeRequest

	err := json.Unmarshal(body, &t)

	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	clientsecret := os.Getenv("AUTH0_PORTAL_CLIENT_SECRET")
	redirecturl := os.Getenv("AUTH0_PORTAL_CLI_REDIRECT_URL")

	token, name, groups, schema, email, roles, err :=  authentication.GetTunnelToken(t.Code, domain, clientid, clientsecret, redirecturl) 
	var ret types.AuthorizationCodeResponse
	ret.Token = token
	ret.Groups = groups 
	ret.Name = name

	session, _ := authentication.GetSessionFromToken(token)

	js, err := json.Marshal(ret)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api AuthByCode, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api AuthByCode successful")
	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)

}
func GetDatascopesAccess(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	//email, groups, _ := authentication.GetIdentityFromToken(token)

	out, _ := authentication.GetDatascopesForGroups(schema, email, groups)

	js, _ := json.Marshal(out)
	log.InfoUserf(schema, session, email, groups, roles, "Api GetDatascopesAccess, success")

	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)
}

func RegenerateDatascopePassword(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	out, err := authentication.RegenerateDatascopePassword(schema, email, groups)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api RegenerateDatascopePassword, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, _ := json.Marshal(out)
	log.InfoUserf(schema, session, email, groups, roles, "Api RegenerateDatascopePassword, success")

	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)
}


func DownloadUpdate(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	os, _ := vars["os"]
	arch, _ := vars["arch"]	

	fil := fmt.Sprintf("./customer/update/%s/%s/tunnel", os, arch)
	log.Info("Api DownloadUpdate called")
	http.ServeFile(w, r, fil)
}

func QueryTunnel(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CustomerIDRequest

	err := json.Unmarshal(body, &t)

	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	redirecturl := os.Getenv("AUTH0_PORTAL_CLI_REDIRECT_URL")

	var schema string
	if( strings.HasPrefix(t.Customerid, "org_") ) {
		schema, err = authentication.GetSchemaFromClientId(t.Customerid)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	} else {
		schema, err = authentication.GetClientIdFromSchema(t.Customerid)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		// flip
		schema,t.Customerid  = t.Customerid, schema
	}
	
	lbaddress := schema + os.Getenv("LB_DOMAIN")
	port := os.Getenv("LB_PORT")
	if(port == "") {
		port = "443"
	}
	lbport, _ :=  strconv.Atoi(port) 

	tunnellogin := fmt.Sprintf("%sauthorize?response_type=code&client_id=%s&redirect_uri=%s&organization=%s&scope=%s",
		domain, clientid, redirecturl, t.Customerid,  url.QueryEscape("openid profile") )

	var resp  types.CustomerIDResponse
	
	resp.LoginURL = tunnellogin
	resp.Lbaddress = lbaddress
	resp.Lbport = lbport

	resp.ProtocolVersion = os.Getenv("PROTOCOL_VERSION")
	resp.ClientMajorVersion = os.Getenv("MAJOR_VERSION")
	resp.ClientMinorVersion = os.Getenv("MINOR_VERSION")


	js, err := json.Marshal(resp)

	
	if err != nil {
		log.Debugf("QueryTunnel error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.Info("Api QueryTunnel called")
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)

}
// convert DER to PEM format
func pemCert(derBytes []byte) []byte {
	pemBlock := &pem.Block{
		Type:    "CERTIFICATE",
		Headers: nil,
		Bytes:   derBytes,
	}
	out := pem.EncodeToMemory(pemBlock)
	return out
}

func GetClientCertificate(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CertificateRequest

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error unmarshaling cert: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
    
	pemBlock, _ := pem.Decode( []byte(t.Csr) )
    if pemBlock == nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, pem.Decode failed")
		http.Error(w, "pem.Decode failed", http.StatusInternalServerError)
		return		
    }

	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes) 
    if err = clientCSR.CheckSignature(); err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
    }

	// create client certificate template
    clientCRTTemplate := x509.Certificate{
        Signature:          clientCSR.Signature,
        SignatureAlgorithm: clientCSR.SignatureAlgorithm,

        PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
        PublicKey:          clientCSR.PublicKey,

        SerialNumber: big.NewInt(2),
        Issuer:       authentication.CaCert.Subject,
        Subject:      clientCSR.Subject,
        NotBefore:    time.Now().Add(-600 * time.Second), // grace time
        NotAfter:     time.Now().Add(600 * time.Second), // REMOVE ME extra 0
        KeyUsage:     x509.KeyUsageDigitalSignature,
        ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames: clientCSR.DNSNames,
    }

    // create client certificate from template and CA public key
    clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, authentication.CaCert, 
		clientCSR.PublicKey, authentication.CaKey)
	
    if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
    }

    out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	var certout types.CSRResponse 
	certout.Certificate = string(out)
	js, err := json.Marshal(certout)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetClientCertificate, success")

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(js))
}

// TODO TODO TODO TODO TODO TODO TODO TODO TODO
// check the secret here!!!
func GetConnectorCertificate(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CertificateRequestWithSecret

	err := json.Unmarshal(body, &t)
	schema := t.Customer
	key := t.Key
	secret := t.Secret

	aerr := authentication.CheckConnectorAuth(schema, key, secret)
	if aerr != nil {
		http.Error(w, aerr.Error(), http.StatusInternalServerError)
		return
	}
	//fmt.Printf("schema: %s, key: %s, secret %s\n", schema, key, secret)

	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error unmarshaling cert: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
    
	pemBlock, _ := pem.Decode( []byte(t.Csr) )
    if pemBlock == nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, pem.Decode failed")
		http.Error(w, "pem.Decode failed", http.StatusInternalServerError)
		return		
    }

	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes) 
    if err = clientCSR.CheckSignature(); err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
    }

	targets, err := authentication.GetTargets(clientCSR.Subject.CommonName, key, secret)

	// create client certificate template
    clientCRTTemplate := x509.Certificate{
        Signature:          clientCSR.Signature,
        SignatureAlgorithm: clientCSR.SignatureAlgorithm,

        PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
        PublicKey:          clientCSR.PublicKey,

        SerialNumber: big.NewInt(2),
        Issuer:       authentication.CaCert.Subject,
        Subject:      clientCSR.Subject,
        NotBefore:    time.Now().Add(-600 * time.Second), // grace time
        NotAfter:     time.Now().Add(600 * time.Second), // DELETE ME
        KeyUsage:     x509.KeyUsageDigitalSignature,
        ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames: 		targets,
    }

    // create client certificate from template and CA public key
    clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, authentication.CaCert, 
		clientCSR.PublicKey, authentication.CaKey)
	
    if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
    }

    out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	var certout types.CSRResponse 
	certout.Certificate = string(out)
	js, err := json.Marshal(certout)
	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoTenantf(schema, "Api GetConnectorCertificate, success")

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(js))
}


func GetConnections(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	connections, error := authentication.GetConnections(schema)
	if(error != nil) {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetConnections: %s", error.Error())
		var status  types.OperationStatus
		status = types.OperationStatus{"Error", error.Error()}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	var status types.ConnectionResponse
	status = types.ConnectionResponse{"OK", "", connections}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetConnections: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	} else {
		var out []string
		for i := 0; i < len(connections); i++ {
			out = append(out, connections[i].Name)
		}
		log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetConnections, success", out)

	}

	w.Write(js)
}

func DatascopeHelp(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	token, _ := vars["token"]
	sport, _ := vars["port"]
	
	newtoken, error := authentication.CheckAndRefreshToken(token, sport)

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	if(error != nil) {
		http.Error(w, error.Error(), http.StatusNotFound)
		return
	}

	js := []byte(`<html>
	<head>
	<script>
	 !function() {
		sessionStorage.setItem("Session", "`+newtoken+`")
		window.location.href = "/app/access?key=datascopes"
	 }()
	</script>
	</head>
	<body>Callback arrived</body>
	</html>`)

	w.Write(js)
}
func GetImages(w http.ResponseWriter, r *http.Request) {
	filename := path.Join(authentication.FilesystemRoot, "./customer/" + r.URL.Path)

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
			Commonheaders(w, r)
		}
		http.ServeFile(w, r, filename)
	}
}

func GetAccessKeys(w http.ResponseWriter, r *http.Request) {
	var out types.GetKeySecret
	out.Accesskey, _ = authentication.GenerateRandomString(12)
	out.Secret, _ = authentication.GenerateRandomString(128)

	js, err := json.Marshal(out)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func CreateNewConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.AddConnectorRequest{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := authentication.CreateNewConnector(schema, &t)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	status := types.AuthStatus{"OK", "Connector "+t.Name+" provisioned!", id}
	js, err := json.Marshal(status)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))	
}

func GetConnectors(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	conns, err := authentication.GetConnectors(schema)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	js, err := json.Marshal(conns)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return		
	}
	if len(conns) == 0 {
		js = []byte("[]")
	}
	fmt.Printf("%s \n", string(js))
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))		
}

func UpdateConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.AddConnectorRequest{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.UpdateConnector(schema, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	status := types.OperationStatus{"OK", "Connector "+t.Name+" updated!"}
	js, err := json.Marshal(status)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))		
}

func DeleteConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DatascopeId{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.DeleteConnector(schema, t.Id)

	status := types.OperationStatus{"OK", "Connector deleted!"}
	if err != nil {
		status = types.OperationStatus{"Error", err.Error()}
	}
	js, err := json.Marshal(status)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))		
}