// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
package dhandlers

import (
	"crypto/rand"
	"crypto/x509"
	"encoding/json"
	"encoding/pem"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/big"
	"net/http"
	"net/url"
	"os"
	"path"
	"strconv"
	"strings"
	"time"
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/certificates"
	"dymium.com/dymium/common"
	"dymium.com/dymium/gotypes"
	"dymium.com/dymium/log"
	"dymium.com/dymium/types"
	"dymium.com/server/protocol"
	"github.com/Jeffail/gabs"
	"github.com/golang-jwt/jwt"
	"github.com/gorilla/mux"
	_ "github.com/gorilla/mux"
	"golang.org/x/net/context"
)

type contextKey int

const authenticatedSchemaKey contextKey = 0
const authenticatedEmailKey contextKey = 1
const authenticatedGroupsKey contextKey = 2
const authenticatedOrgKey contextKey = 3
const authenticatedRolesKey contextKey = 4
const authenticatedSessionKey contextKey = 5
const authenticatedDomainKey contextKey = 6

const GRACE = 300 // +- seconds for the cert to be valid. 5 min is a little bit generous

func AuthMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
		token := common.TokenFromHTTPRequest(r)
		if token == "" {
			rr := "Auth Error: absent token"
			log.Errorf(rr)
			http.Error(w, rr, http.StatusForbidden)
			return
		}
		schema, roles, groups, email, orgid, session, error := authentication.GetSchemaRolesFromToken(token)

		if error != nil {
			log.Errorf("Auth Error: %s", error.Error())
			http.Error(w, error.Error(), http.StatusForbidden)
			return
		}
		//create a new request context containing the authenticated user
		ctxWithSchema := context.WithValue(r.Context(), authenticatedSchemaKey, schema)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedGroupsKey, groups)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedEmailKey, email)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedOrgKey, orgid)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedRolesKey, roles)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedSessionKey, session)
		ctxWithSchema = context.WithValue(ctxWithSchema, authenticatedDomainKey, session)


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
			common.CommonNocacheHeaders(w, r)
			w.Header().Set("Content-Type", "application/json")
			w.Write(js)
			return
		}
		h.ServeHTTP(w, rWithSchema)
	})
}

func InviteMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
		token := common.TokenFromHTTPRequest(r)
		if token == "" {
			rr := "Auth Error: absent token"
			log.Errorf(rr)
			http.Error(w, rr, http.StatusForbidden)
			return
		}
		jwtKey := []byte(os.Getenv("SESSION_SECRET"))
		claim := &gotypes.Claims{}
	
		tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
			return jwtKey, nil
		})
		if nil == err {
			if !tkn.Valid {
				rr := "Authentication failed"
				log.Errorf(rr)
				http.Error(w, rr, http.StatusForbidden)
				return 
			}
		} else {
			log.Errorf(err.Error())
			http.Error(w, err.Error(), http.StatusForbidden)
			return 
		}

		use := authentication.Authorized(r, claim.Roles)
		if !use {
			status := types.OperationStatus{"AuthError", "Role is not authorized for this resource"}
			js, err := json.Marshal(status)

			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			common.CommonNocacheHeaders(w, r)
			w.Header().Set("Content-Type", "application/json")
			w.Write(js)
			return
		}
		h.ServeHTTP(w, r)
	})
}

func QueryTable(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	sessions := r.Context().Value(authenticatedSessionKey).(string)

	status := types.ConnectionDetailResponse{"OK", "", nil}
	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.TableQuery{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryTable Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// get the connection details
	conn, _, err := authentication.GetConnection(schema, t.ConnectionId)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryTable Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	} else {
		log.InfoUserf(schema, sessions, email, groups, roles, "Api QueryTable %s, success", conn.Database)
	}

	arr, err := authentication.GetPolicies(schema)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryTable, GetPolicy Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	// now get the PIIDetectors
	var policy types.DataPolicy
	err = json.Unmarshal(arr, &policy)
	var detectors []types.PIIDetector
	for _, p := range policy.Piisuggestions {
		detectors = append(detectors, p.Detector)
	}
	params := types.TableInfoParams{t.Schema, t.Table, detectors}

	arequest := types.AnalyzerRequest{types.DT_TableInfo, conn, &params}
	bconn, _ := json.Marshal(arequest)

	invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryTable Error: %s", err.Error())
		rr := fmt.Sprintf("Unable to retrieve information from the data source. %s", err.Error())
		status = types.ConnectionDetailResponse{"Error", rr, nil}

	} else {
		jsonParsed, err := gabs.ParseJSON(invokebody)
		if err != nil {
			log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryTable Error parsing output: %s", err.Error())
		}
		value, ok := jsonParsed.Path("Errormessage").Data().(string)
		if ok {
			rr := fmt.Sprintf("Api Error in QueryTable Invoke return: %s", value)
			status = types.ConnectionDetailResponse{"Error", rr, nil}

		} else {
			t := types.AnalyzerResponse{}
			err := json.Unmarshal(invokebody, &t)
			if err != nil {
				status = types.ConnectionDetailResponse{"Error", err.Error(), nil}
			} else {
				status = types.ConnectionDetailResponse{"OK", "", &t}
			}
		}
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
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
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api QueryConnection Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	// get the connection details
	conn, use_connector, err := authentication.GetConnection(schema, t.ConnectionId)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api Error: %s", err.Error())
	} else {
		log.InfoUserf(schema, sessions, email, groups, roles, "Api QueryConnection %s, success", conn.Database)
	}

	arequest := types.AnalyzerRequest{Dtype: types.DT_DatabaseInfo, Connection: conn}
	bconn, err := json.Marshal(arequest)

	invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
	if err != nil {
		log.ErrorUserf(schema, sessions, email, groups, roles, "Api DbAnalyzer Error: %s", err.Error())
		if use_connector {
			status = types.ConnectionDetailResponse{"Error", "Unable to establish connection. Check if the connector is running, and configured properly", nil}
		} else {
			status = types.ConnectionDetailResponse{"Error", "Unable to establish connection to the data source", nil}
		}

	} else {
		jsonParsed, err := gabs.ParseJSON(invokebody)
		if err != nil {
			log.ErrorUserf(schema, sessions, email, groups, roles, "Api DbAnalyzer Error parsing output: %s", err.Error())
		}
		value, ok := jsonParsed.Path("Errormessage").Data().(string)
		if ok {
			rr := fmt.Sprintf("Api Error in DbAnalyzer Invoke return: %s", value)
			status = types.ConnectionDetailResponse{"Error", rr, nil}

		} else {
			t := types.AnalyzerResponse{}
			err := json.Unmarshal(invokebody, &t)
			if err != nil {
				status = types.ConnectionDetailResponse{"Error", err.Error(), nil}
			} else {
				status = types.ConnectionDetailResponse{"OK", "", &t}
			}
		}
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveDatascope, error %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.SaveDatascope(schema, t)
	var status types.OperationStatus
	if error == nil {
		status = types.OperationStatus{"OK", "Ghost Database created"}
		log.InfoUserf(schema, session, email, groups, roles, "Api SaveDatascope(%s), success", t.Name)

	} else {
		status = types.OperationStatus{"Error", error.Error()}
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveDatascope, error %s", error.Error())

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "application/json")
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
		w.Header().Set("Content-Type", "application/json")
		w.Write(js)
		return
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if err != nil {
		log.Errorf("Api SaveDatascope, DbSync Error parsing output: %s", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if ok {
		rr := fmt.Sprintf("Api SaveDatascope, Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.Errorf("Api %s", rr)
	}

	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.RefreshMachineTunnels(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveDatascope, RefreshMachineTunnels returned: %s", err.Error())
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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

	bytesin, bytesout, logins, tunnels, err := authentication.GetBytes(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, GetBytes error: %s", err.Error())
		//http.Error(w, err.Error(), http.StatusInternalServerError)
		//return
	}

	var t types.Usage

	t.Bytesin = strconv.FormatInt(bytesin, 10)
	t.Bytesout = strconv.FormatInt(bytesout, 10)
	t.Logins = logins //
	t.Tunnels = tunnels

	connections, datascopes, blocked, obfuscated, redacted, connectors, ctunnels, err := authentication.GetRestrictions(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, GetRestrictions error: %s", err.Error())

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
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetUsage, Marshal error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetUsage, success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	ds, err := authentication.GetSelect(schema, groups, roles, &t)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	js, err := json.Marshal(ds)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetSelect, error %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetSelect, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api QueryConnection Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var status types.DatascopeTables
	ds, err := authentication.GetDatascopeTables(schema, t.Id)

	if err != nil {
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
	for i := 0; i < len(ds); i++ {
		outs = append(outs, ds[i].Table)
	}
	log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetDatascapeTables, success", outs)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascopeDetails Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var status types.DatascopeInfoStatus
	ds, err := authentication.GetDatascope(schema, t.Id)
	if err != nil {
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, marshaling error %ss", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if t.Usesconnector {
		t.Address, t.Port, err = authentication.GetConnectorAddress(schema, t.Tunnelid)
	}
	if err == nil {
		conn, use_connector, err := authentication.GetConnection(schema, *t.Id)
		if err != nil {
			log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, error %s, Id %s", err.Error(), *t.Id)
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		conn.Typ = types.ConnectionType(t.Dbtype)
		conn.Address = t.Address
		conn.Port = t.Port
		conn.Database = t.Dbname
		conn.Tls = t.UseTLS

		if t.Username != nil && t.Password != nil {
			conn.User = *t.Username
			conn.Password = *t.Password
		}
		arequest := types.AnalyzerRequest{Dtype: types.DT_Test, Connection: conn}

		bconn, err := json.Marshal(arequest)
		invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
		if err != nil {
			log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, DbAnalyzer error: %s", err.Error())
			var rr string
			if use_connector {
				rr = "Unable to establish connection. Check if the connector is running, and configured properly"
			} else {
				rr = "Unable to establish connection to the data source"
			}
			http.Error(w, rr, http.StatusInternalServerError)
			return
		} else {
			jsonParsed, err := gabs.ParseJSON(invokebody)
			if err != nil {
				log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, DbAnalyzer error parsing output: %s", err.Error())
			}
			value, ok := jsonParsed.Path("Errormessage").Data().(string)
			if ok {
				rr := fmt.Sprintf("UpdateConnection, Error in Invoke return: %s", value)
				log.ErrorUserf(schema, session, email, groups, roles, "Api %s", rr)
				http.Error(w, rr, http.StatusInternalServerError)
				return
			}
		}
	}

	error := authentication.UpdateConnection(schema, t)

	if error != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnection, error: %s", error.Error())
		http.Error(w, error.Error(), http.StatusInternalServerError)
		return
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteMapping Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.DeleteMapping(schema, t.Id)

	var status types.OperationStatus
	if error != nil {
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
	if error == nil {
		log.InfoUserf(schema, session, email, groups, roles, "Api DeleteMapping, success")
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateMapping Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error, admincount := authentication.UpdateMapping(schema, *t.Id, t.Dymiumgroup, t.Directorygroup, t.Comments, t.Adminaccess)
	var status types.GroupStatus
	if error == nil {
		status = types.GroupStatus{"OK", "Mapping updated", admincount}
	} else {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateMapping, error: %s", error.Error())
		status = types.GroupStatus{"Error", error.Error(), 0}
	}
	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateMapping, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if error == nil {
		log.InfoUserf(schema, session, email, groups, roles, "Api UpdateMapping, success")
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api CreateMapping Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.CreateNewMapping(schema, t.Dymiumgroup, t.Directorygroup, t.Comments, t.Adminaccess)
	var status types.OperationStatus
	if error == nil {
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
	if error == nil {
		log.InfoUserf(schema, session, email, groups, roles, "Api CreateMapping %s to %s, success", t.Directorygroup, t.Dymiumgroup)
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api CreateNewConnection Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id, error := authentication.CreateNewConnection(schema, t)
	var status types.OperationStatus
	if error == nil {
		status = types.OperationStatus{"OK", "Connection created"}
	} else {
		status = types.OperationStatus{"Error", error.Error()}
		log.ErrorUserf(schema, session, email, groups, roles, "Error: %s", error.Error())
	}

	if error == nil {
		// get the connection details
		conn, use_connector, err := authentication.GetConnection(schema, id)
		log.Infof("schema: %s, id: %s", schema, id)

		if err == nil {
			log.InfoUserf(schema, session, email, groups, roles, "Api Connection to %s created", conn.Database)

			arequest := types.AnalyzerRequest{Dtype: types.DT_Test, Connection: conn}
			bconn, err := json.Marshal(arequest)

			log.Infof("Api CreateNewConnection, conn: %s", string(bconn))
			invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
			if err != nil {
				log.ErrorUserf(schema, session, email, groups, roles, "Api DbAnalyzer Error: %s", err.Error())
				if use_connector {
					status = types.OperationStatus{"Error", "Unable to establish connection. Check if the connector is running, and configured properly"}
				} else {
					status = types.OperationStatus{"Error", "Unable to establish connection to the data source"}
				}
			} else {
				jsonParsed, err := gabs.ParseJSON(invokebody)
				if err != nil {
					log.ErrorUserf(schema, session, email, groups, roles, "Api DbAnalyzer Error parsing output: %s", err.Error())
				}
				value, ok := jsonParsed.Path("Errormessage").Data().(string)
				if ok {
					rr := fmt.Sprintf("Error in Invoke return: %s", value)
					status = types.OperationStatus{"Error", rr}
					log.ErrorUserf(schema, session, email, groups, roles, "Api GetConnection error %s", rr)
				}
			}
		} else {
			log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetConnection to %s: %s", conn.Database, err.Error())
		}
	}
	if status.Status == "Error" {
		authentication.DeleteConnection(schema, id)
		log.ErrorUserf(schema, session, email, groups, roles, "Api Roll back connection creation")
	}

	js, err := json.Marshal(status)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetConnection Marshal error %s",
			err.Error())

		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, unmarshaling error: %s", err.Error())
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
		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "application/json")
		w.Write(js)
		return
	}

	var rq types.Request
	rq.Action = types.A_Update
	if len(t.Records) == 0 {
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
		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "application/json")
		w.Write(js)
		return
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, DbSync Error parsing output: %s", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if ok {
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
	err = authentication.RefreshMachineTunnels(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateDatascope, RefreshMachineTunnels returned: %s", err.Error())
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, unmarshaling error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	var rq types.Request
	rq.Action = types.A_Delete

	rq.Customer = schema
	rq.Datascope = &t.Name
	snc, err := json.Marshal(rq)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, marshaling error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	invokebody, err := authentication.Invoke("DbSync", nil, snc)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, DbSync Invoke Error: %s", err.Error())
		status = types.OperationStatus{"Error", err.Error()}

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "application/json")
		w.Write(js)
		return
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteDatascope, DbSync Error parsing output: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	var js []byte
	if ok {
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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

	var status types.OperationStatus
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
	if error == nil {
		log.InfoUserArrayf(schema, session, email, groups, roles, "Api SaveGroups for Datascope %s, success", out, t.Name)
	}
	err = authentication.RefreshMachineTunnels(schema)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api SaveGroups, RefreshMachineTunnels returned: %s", err.Error())
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteConnection Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	conn, _, err := authentication.GetConnection(schema, t.Id)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in DeleteConnection: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	err = authentication.DeleteConnection(schema, t.Id)
	var status types.OperationStatus
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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

	var status types.OperationStatus
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

	common.CommonNocacheHeaders(w, r)
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetDatascopesForTestSQL(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	datascopes, error := authentication.GetDatascopesForTestSQL(schema, roles, groups)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")

	if error != nil {
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

func GetDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	datascopes, error := authentication.GetDatascopes(schema)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")

	if error != nil {
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

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")

	mappings, error := authentication.GetMappings(schema)
	if error != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetMappings, error: %s", error.Error())

		var status types.GroupMappingStatus
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
		s := fmt.Sprintf("%s to %s, ", mappings[i].Directorygroup, mappings[i].Dymiumgroup)
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

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")

	mappings, error := authentication.GetGroupAssignments(schema)
	if error != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetGroupsForDatascopes, error: %s", error.Error())
		var status types.OperationStatus
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
		s := fmt.Sprintf("%s available to %s, ", mappings[i].Name, mappings[i].Groupname)
		out = append(out, s)
	}

	log.InfoUserArrayf(schema, session, email, groups, roles, "Api GetGroupsForDatascopes, success", out)
	w.Write(js)
}
func FakeLogin(w http.ResponseWriter, r *http.Request) {

	if r.Host != "portal.dymium.local" || r.Host != os.Getenv("CUSTOMER_HOST") {
		log.Errorf("Error: FakeLogin on non-local host requested")
		http.Error(w, "Fake login forbidden", http.StatusInternalServerError)
		return
	}
	js := authentication.GetFakeAuthentication()
	log.Infof("FakeLogin on local host,  success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	http.Redirect(w, r, logoutURL, 302)

}

func AuthByCode(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.AuthorizationCodeRequest

	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	clientsecret := os.Getenv("AUTH0_PORTAL_CLIENT_SECRET")
	redirecturl := os.Getenv("AUTH0_PORTAL_CLI_REDIRECT_URL")

	token, name, groups, schema, email, roles, err := authentication.GetTunnelToken(t.Code, domain, clientid, clientsecret, redirecturl)
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
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)

}
func GetDatascopesAccess(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	domain := r.Context().Value(authenticatedDomainKey).(string)

	//email, groups, _ := authentication.GetIdentityFromToken(token)
fmt.Printf("schema %s, email %s, groups %s\n", schema, email, groups)
	out, err := authentication.GetDatascopesForGroups(schema, email, groups, domain)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetDatascopesAccess, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	js, _ := json.Marshal(out)
	log.InfoUserf(schema, session, email, groups, roles, "Api GetDatascopesAccess, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func RegenerateDatascopePassword(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	domain := r.Context().Value(authenticatedDomainKey).(string)

	out, err := authentication.RegenerateDatascopePassword(schema, email, groups, domain)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api RegenerateDatascopePassword, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, _ := json.Marshal(out)
	log.InfoUserf(schema, session, email, groups, roles, "Api RegenerateDatascopePassword, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func DownloadUpdate(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	os := vars["os"]
	arch := vars["arch"]

	fil := fmt.Sprintf("/%s/%s/tunnel", os, arch)

	authentication.StreamFromS3(w, r, getClientBucket(), fil)
}

func DownloadConnectorUpdate(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	os := vars["os"]
	arch := vars["arch"]

	fil := fmt.Sprintf("/%s/%s/meshconnector", os, arch)

	authentication.StreamFromS3(w, r, getConnectorBucket(), fil)

}

func DownloadMachineClientUpdate(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	os := vars["os"]
	arch := vars["arch"]

	fil := fmt.Sprintf("/%s/%s/machineclient", os, arch)
log.Debugf("DownloadMachineClientUpdate, fil: %s", fil)
	authentication.StreamFromS3(w, r, getMachineClientBucket(), fil)
}

func QueryTunnel(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CustomerIDRequest

	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
	clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	redirecturl := os.Getenv("AUTH0_PORTAL_CLI_REDIRECT_URL")

	var schema string
	if strings.HasPrefix(t.Customerid, "org_") {
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
		schema, t.Customerid = t.Customerid, schema
	}

	lbaddress := schema + os.Getenv("LB_DOMAIN")
	port := os.Getenv("LB_PORT")
	if port == "" {
		port = "443"
	}
	lbport, _ := strconv.Atoi(port)

	tunnellogin := fmt.Sprintf("%sauthorize?response_type=code&client_id=%s&redirect_uri=%s&organization=%s&scope=%s&prompt=login",
		domain, clientid, redirecturl, t.Customerid, url.QueryEscape("openid profile email"))

	var resp types.CustomerIDResponse

	resp.LoginURL = tunnellogin
	resp.Lbaddress = lbaddress
	resp.Lbport = lbport

	resp.Version = protocol.TunnelServerVersion

	js, err := json.Marshal(resp)

	if err != nil {
		log.Debugf("QueryTunnel error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.Info("Api QueryTunnel, success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
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

	pemBlock, _ := pem.Decode([]byte(t.Csr))
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
		Issuer:       certificates.CaCert.Subject,
		Subject:      clientCSR.Subject,
		NotBefore:    time.Now().Add(-GRACE * time.Second), // grace time
		NotAfter:     time.Now().Add(GRACE * time.Second),  // REMOVE ME extra 0
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames:     clientCSR.DNSNames,
	}

	// create client certificate from template and CA public key
	clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, certificates.CaCert,
		clientCSR.PublicKey, certificates.CaKey)

	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}

	out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	var certout types.CSRResponse
	certout.Certificate = string(out)
	certout.Version = protocol.TunnelServerVersion
	js, err := json.Marshal(certout)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api GetClientCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetClientCertificate, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

// check the secret here!!!
func GetConnectorCertificate(w http.ResponseWriter, r *http.Request) {

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CertificateRequestWithSecret

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Errorf("Api GetConnectorCertificate, error unmarshaling cert: %s", err.Error())
		http.Error(w, "Invalid request", http.StatusInternalServerError)
		return
	}
	schema := t.Customer
	key := t.Key
	secret := t.Secret

	aerr := authentication.CheckConnectorAuth(schema, key, secret)
	if aerr != nil {
		http.Error(w, "There is no record of a connector with this configuration", http.StatusInternalServerError)
		return
	}
	//fmt.Printf("schema: %s, key: %s, secret %s\n", schema, key, secret)

	pemBlock, _ := pem.Decode([]byte(t.Csr))
	if pemBlock == nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, pem.Decode failed")
		http.Error(w, "Certificate Request has invalid encoding", http.StatusInternalServerError)
		return
	}

	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes)
	if err = clientCSR.CheckSignature(); err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, "Certificate Request has invalid format", http.StatusInternalServerError)
		return
	}

	targets, err := authentication.GetTargets(clientCSR.Subject.CommonName, key, secret)
	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, "Dymium failed to get targets", http.StatusInternalServerError)
		return
	}

	// create client certificate template
	clientCRTTemplate := x509.Certificate{
		Signature:          clientCSR.Signature,
		SignatureAlgorithm: clientCSR.SignatureAlgorithm,

		PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
		PublicKey:          clientCSR.PublicKey,

		SerialNumber: big.NewInt(2),
		Issuer:       certificates.CaCert.Subject,
		Subject:      clientCSR.Subject,
		NotBefore:    time.Now().Add(-GRACE * time.Second), // grace time
		NotAfter:     time.Now().Add(GRACE * time.Second),  // DELETE ME
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames:     targets,
	}

	// create client certificate from template and CA public key
	clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, certificates.CaCert,
		clientCSR.PublicKey, certificates.CaKey)

	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, "Dymium failed to create a client cert", http.StatusInternalServerError)
		return
	}

	out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	var certout types.CSRResponse
	certout.Certificate = string(out)
	certout.Version = protocol.MeshServerVersion
	js, err := json.Marshal(certout)
	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoTenantf(schema, "Api GetConnectorCertificate, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func GetMachineClientCertificate(w http.ResponseWriter, r *http.Request) {
	// get a cert based on csr authenticating against machine access ket/secret, similar to previous function
	// but with machine access key/secret

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CertificateRequestWithSecret

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Errorf("Api GetMachineClientCertificate, error unmarshaling cert: %s", err.Error())
		http.Error(w, "Invalid request", http.StatusInternalServerError)
		return
	}
	schema := t.Customer
	key := t.Key
	secret := t.Secret

	groups, token, aerr := authentication.AuthenticateAndPrepareMachineTunnel(schema, key, secret)
	if aerr != nil {
		log.ErrorTenantf(schema, "Api GetMachineClientCertificate. There is no record of machine tunnel with this configuration")
		http.Error(w, "There is no record of machine tunnel with this configuration", http.StatusInternalServerError)
		return
	}
	//fmt.Printf("schema: %s, key: %s, secret %s\n", schema, key, secret)

	pemBlock, _ := pem.Decode([]byte(t.Csr))
	if pemBlock == nil {
		log.ErrorTenantf(schema, "Api GetMachineClientCertificate, pem.Decode failed")
		http.Error(w, "Certificate Request has invalid encoding", http.StatusInternalServerError)
		return
	}

	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes)
	if err = clientCSR.CheckSignature(); err != nil {
		log.ErrorTenantf(schema, "Api GetMachineClientCertificate, error: %s", err.Error())
		http.Error(w, "Certificate Request has invalid format", http.StatusInternalServerError)
		return
	}

	// create client certificate template
	clientCRTTemplate := x509.Certificate{
		Signature:          clientCSR.Signature,
		SignatureAlgorithm: clientCSR.SignatureAlgorithm,

		PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
		PublicKey:          clientCSR.PublicKey,

		SerialNumber: big.NewInt(2),
		Issuer:       certificates.CaCert.Subject,
		Subject:      clientCSR.Subject,
		NotBefore:    time.Now().Add(-GRACE * time.Second), // grace time
		NotAfter:     time.Now().Add(GRACE * time.Second),  // DELETE ME
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames:     groups,
	}

	// create client certificate from template and CA public key
	clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, certificates.CaCert,
		clientCSR.PublicKey, certificates.CaKey)

	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, "Dymium failed to create a client cert", http.StatusInternalServerError)
		return
	}

	out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})

	var certout types.MachineCSRResponse
	certout.Certificate = string(out)
	certout.Jwt = token
	certout.Version = protocol.MeshServerVersion

	js, err := json.Marshal(certout)
	if err != nil {
		log.ErrorTenantf(schema, "Api GetConnectorCertificate, error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoTenantf(schema, "Api GetConnectorCertificate, success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

// check the secret here!!!
func SetConnectorStatus(w http.ResponseWriter, r *http.Request) {

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.SetConnectorStatus

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Errorf("Api SetConnectorStatus, error unmarshaling cert: %s", err.Error())
		http.Error(w, "Invalid request", http.StatusInternalServerError)
		return
	}
	schema := t.Customer
	key := t.Key
	secret := t.Secret

	aerr := authentication.CheckConnectorAuth(schema, key, secret)
	if aerr != nil {
		http.Error(w, aerr.Error(), http.StatusInternalServerError)
		return
	}
	status := t.Status

	authentication.SetConnectorStatus(schema, status)
	log.Info("Api SetConnectorStatus, success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte("status updated"))
}

func GetConnections(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")

	connections, error := authentication.GetConnections(schema)
	if error != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api Error in GetConnections: %s", error.Error())
		var status types.OperationStatus
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
	nonce, _ := certificates.GenerateRandomString(32)
	common.CommonNocacheNocspHeaders(w, r)
	w.Header().Set("Content-Type", "text/html")
	w.Header().Set("x-content-type-options", "nosniff")
	w.Header().Set("strict-transport-security", "max-age=31536000")
	w.Header().Set("X-Frame-Options", "sameorigin")
	w.Header().Set("Content-Security-Policy", "script-src 'nonce-"+nonce+"'")
	if error != nil {
		http.Error(w, error.Error(), http.StatusNotFound)
		return
	}

	js := []byte(`<html>
	<head>
	<script nonce="` + nonce + `">
	 !function() {
		sessionStorage.setItem("Session", "` + newtoken + `")
		window.location.href = "/app/access?key=datascopes"
	 }()
	</script>
	</head>
	<body>Callback arrived</body>
	</html>`)

	w.Write(js)
}
func GetImages(w http.ResponseWriter, r *http.Request) {
	filename := path.Join(authentication.FilesystemRoot, "./customer/"+r.URL.Path)

	if _, err := os.Stat(filename); errors.Is(err, os.ErrNotExist) {
		// file does not exist
		common.CommonNocacheHeaders(w, r)
		w.Header().Set("Content-Type", "text/html")
		w.WriteHeader(http.StatusNotFound)

		io.WriteString(w, "<html><body>Dymium Error 404, file not found</body></html>")
	} else {
		// file exists
		if strings.HasPrefix(r.URL.Path, "/static") || strings.HasSuffix(r.URL.Path, ".png") || strings.HasSuffix(r.URL.Path, "*.gif") ||
			strings.HasSuffix(r.URL.Path, ".jpg") || strings.HasSuffix(r.URL.Path, ".svg") ||
			strings.HasSuffix(r.URL.Path, ".tgz") || strings.HasSuffix(r.URL.Path, ".zip") {

			w.Header().Set("Cache-Control", "public, max-age=3600, immutable")
			w.Header().Set("x-content-type-options", "nosniff")
			w.Header().Set("strict-transport-security", "max-age=31536000")
			w.Header().Set("Content-Security-Policy", "frame-ancestors none")
			w.Header().Set("X-Frame-Options", "sameorigin")
		} else {
			common.CommonNocacheHeaders(w, r)
		}
		http.ServeFile(w, r, filename)
	}
}

func GetAccessKeys(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	var out types.GetKeySecret
	out.Accesskey, _ = certificates.GenerateRandomString(12)
	out.Secret, _ = certificates.GenerateRandomString(128)

	js, err := json.Marshal(out)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetAccessKeys, success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func CreateNewConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.AddConnectorRequest{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api CreateNewConnector Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := authentication.CreateNewConnector(schema, &t)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	status := types.AuthStatus{"OK", "Connector " + t.Name + " provisioned!", id}
	js, err := json.Marshal(status)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func GetConnectors(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

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
	log.InfoUserf(schema, session, email, groups, roles, "Api GetConnectors success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func MachineClientStatus(w http.ResponseWriter, r *http.Request) {
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte("OK"))
}

func UpdateConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.AddConnectorRequest{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api UpdateConnector Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.UpdateConnector(schema, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	status := types.OperationStatus{"OK", "Connector " + t.Name + " updated!"}
	js, _ := json.Marshal(status)

	log.InfoUserf(schema, session, email, groups, roles, "Api UpdateConnector success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func DeleteConnector(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DatascopeId{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.ErrorUserf(schema, session, email, groups, roles, "Api DeleteConnector Error: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.DeleteConnector(schema, t.Id)

	status := types.OperationStatus{"OK", "Connector deleted!"}
	if err != nil {
		status = types.OperationStatus{"Error", err.Error()}
	}
	js, _ := json.Marshal(status)
	log.InfoUserf(schema, session, email, groups, roles, "Api DeleteConnector success")

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(js))
}

func AddMachineTunnel(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.MachineTunnel{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	o, err := authentication.AddMachineTunnel(schema, t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js := []byte(`{"status": "OK", "id": "` + o + `"}`)
	log.InfoUserf(schema, session, email, groups, roles, "Api AddMachineTunnel success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetMachineTunnels(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	tunnels, err := authentication.GetMachineTunnels(schema)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, err := json.Marshal(tunnels)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if len(tunnels) == 0 {
		js = []byte("[]")
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api GetMachineTunnels success")
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func UpdateMachineTunnel(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.MachineTunnel{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.UpdateMachineTunnel(schema, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	
	err = authentication.RefreshMachineTunnels(schema)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api UpdateMachineTunnel success")
	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func DeleteMachineTunnel(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email := r.Context().Value(authenticatedEmailKey).(string)
	groups := r.Context().Value(authenticatedGroupsKey).([]string)
	roles := r.Context().Value(authenticatedRolesKey).([]string)
	session := r.Context().Value(authenticatedSessionKey).(string)
	
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DatascopeId{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.DeleteMachineTunnel(schema, t.Id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.InfoUserf(schema, session, email, groups, roles, "Api DeleteMachineTunnel success")
	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func RegenMachineTunnel(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DatascopeId{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	err = authentication.RegenMachineTunnel(schema, t.Id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func getClientBucket() string {
	b :=  os.Getenv("CLIENT_BUCKET")
	if b == "" {
		return "dymium-dev-tunneling-clients"
	}
	return b
}
func getConnectorBucket() string {
	b :=  os.Getenv("CONNECTOR_BUCKET")
	if b == "" {
		return "dymium-dev-connectors"
	}
	return b
}
func getMachineClientBucket() string {
	b :=  os.Getenv("MACHINE_CLIENT_BUCKET")
	if b == "" {
		return "dymium-dev-machine-clients"
	}
	return b
}
func getRegistryID() string {
	b :=  os.Getenv("REGISTRY_ID")
	if b == "" {
		return "t0k4e6u4" //dev
	}
	return b
}
func DymiumInstallerExe(w http.ResponseWriter, r *http.Request) {
	authentication.StreamFromS3(w, r, getClientBucket(), "/windows/DymiumInstaller.exe")
}
func DymiumInstallerPkg(w http.ResponseWriter, r *http.Request)  {
	authentication.StreamFromS3(w, r, getClientBucket(), "/macos/DymiumInstaller.pkg")
}
func DymiumInstallerGzip(w http.ResponseWriter, r *http.Request)  {
	authentication.StreamFromS3(w, r, getClientBucket(), "/linux/tunnel.tar.gz")
}

func DymiumDarwinConnector(w http.ResponseWriter, r *http.Request) {
	authentication.StreamFromS3(w, r, getConnectorBucket(), "/darwin/meshconnector_darwin_amd64.tgz")
}
func DymiumLinuxConnector(w http.ResponseWriter, r *http.Request) {
	authentication.StreamFromS3(w, r, getConnectorBucket(), "/linux/meshconnector_linux_amd64.tgz")
}
func DymiumWindowsConnector(w http.ResponseWriter, r *http.Request) {
	authentication.StreamFromS3(w, r, getConnectorBucket(), "/windows/meshconnector_windows_amd64.zip")
}
func RefreshMachineTunnels(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	err := authentication.RefreshMachineTunnels(schema)
	if(err != nil) {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}
func GetDockers(w http.ResponseWriter, r *http.Request) {
	var dockers = types.DockerDownloads{}

	dockers.Meshconnector = os.Getenv("CONNECTOR_DOCKER")
	dockers.Machineclient = os.Getenv("MACHINE_CLIENT_DOCKER")

	if dockers.Meshconnector == "" {
		dockers.Meshconnector = "public.ecr.aws/t0k4e6u4/dymiumconnector:latest"
	}	
	if dockers.Machineclient == "" {
		dockers.Machineclient = "public.ecr.aws/t0k4e6u4/dymiummachinetunnel:latest"
	}

	js, err := json.Marshal(dockers)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}
func GetRegistryId(w http.ResponseWriter, r *http.Request) {
	var id = types.RegistryID{}

	id.Id = getRegistryID()

	js, err := json.Marshal(id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func ProcessInvitation(	w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	inv := vars["inv"]

	str, nonce, err := authentication.ProcessInvitation(inv)
	if err != nil {
		out := `/app/error?header=Invitation%20Expired&body=`+ url.QueryEscape(err.Error())

		http.Redirect(w, r, out, http.StatusTemporaryRedirect)
		return
	}
	js := []byte(str)
	common.CommonNocacheNocspHeaders(w, r)
	w.Header().Set("Content-Security-Policy", "script-src 'nonce-"+nonce+"'")
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}

func TestNameAndLogo(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := struct { 
		Shortname string
		Name string
		Logo string
	}{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	err = authentication.TestNameAndLogo(t.Shortname, t.Name, t.Logo)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}

func TestOIDC(w http.ResponseWriter, r *http.Request) {

	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := struct { 
		Issuer string
		Clientid string
		Secret string
	}{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	
	client := &http.Client{}
	urlStr := fmt.Sprintf("%s/.well-known/openid-configuration", t.Issuer)
	

	nr, err := http.NewRequest(http.MethodGet, urlStr, nil) // URL-encoded payload
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
	}
	nr.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	resp, err := client.Do(nr)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
	}

	defer resp.Body.Close()
	body, err = io.ReadAll(resp.Body)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
	}

	o := struct {
		Issuer string
		Authorization_endpoint string
		Token_endpoint string
		Userinfo_endpoint string
		Jwks_uri string
	} {}
	err = json.Unmarshal(body, &o)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(body)
}

func GetInvitationJson(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})


	js, err := authentication.GetInvitationJson(claim.Session)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}
func PostInvitationJson(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	err := authentication.PostInvitationJson( string(body), claim.Session)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func CreateFootprint(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	err := authentication.StartCreatingFootprint( claim.Session)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}


	js := []byte(`{"status": "OK"}`)
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func CheckFootprintStatus(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	js, err := authentication.CreatingFootprintStatus( claim.Session)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}


	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}	

func ResetInvitedTenant(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	err := authentication.ClearResetTenantFromInvite( claim.Session)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}


	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}	

func InvitationStatus(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	_, _ = jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})

	ret, err := authentication.CheckTenantInvitationStatus( claim.Session)
	if err != nil  {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if !ret  {
		http.Error(w, "Invitation not finished", http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}

func GetOIDCConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	_, issuer, clientid, secret, err := authentication.GetOIDCConnection( schema )
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	co := types.Auth0Connection{Issuer: issuer, Clientid: clientid, Secret: secret}
	js, err := json.Marshal(co)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}	

func GetLoginDetails(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	domain, logo_url, primary, page_background, err := authentication.GetLoginDetails( schema )
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	out := types.AuthLogin{Domain: domain, Logo_url: logo_url, Primary: primary, Page_background: page_background}
	js, err := json.Marshal(out)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func GetSuperAdmins(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	admins, err := authentication.GetSuperAdmins( schema )
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	js, err := json.Marshal(admins)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func SetOIDCConnection(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.Auth0Connection{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	err = authentication.SetOIDCConnection( schema, t.Issuer, t.Clientid, t.Secret)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}

func SetLoginDetails(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.AuthLogin{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	err = authentication.SetLoginDetails( schema, t.Domain, t.Logo_url, t.Primary, t.Page_background)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}

func SetSuperAdmins(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := []string{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Errorf("Api SetSuperAdmins, error unmarshaling cert: %s", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	err = authentication.SetSuperAdmins( schema, t)
	if err != nil {
		log.Errorf("Api SetSuperAdmins, error: %s", err.Error())	
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{"status": "OK"}`))
}

func RegenerateConnectorSecret(w http.ResponseWriter, r *http.Request) {	
	body, _ := io.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.DatascopeId{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	secret, err := authentication.RegenerateConnectorSecret( schema, t.Id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	out := types.GetKeySecret{Accesskey: t.Id, Secret: secret}
	js, err := json.Marshal(out)

	common.CommonNocacheHeaders(w, r)
	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}