//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package dhandlers

import (
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
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
	"log"
	"net/http"
	"strconv"
	"os"
	"strings"
	"path"
	"fmt"
)
type contextKey int
const authenticatedSchemaKey contextKey = 0


func AuthMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
		token := common.TokenFromHTTPRequest(r)
		schema, roles, error := authentication.GetSchemaRolesFromToken(token)
		if error != nil {
			log.Printf("Error: %s\n", error.Error())
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
	status := types.ConnectionDetailResponse{"OK", "", nil}
	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	t := types.ConnectionDetailRequest{}
	err := json.Unmarshal(body, &t)

	// get the connection details
	conn, err := authentication.GetConnection(schema, t.ConnectionId)
	bconn, err := json.Marshal(conn)

	invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
	if err != nil {
		log.Printf("DbAnalyzer Error: %s\n", err.Error())
		status =  types.ConnectionDetailResponse{"Error", err.Error(), nil}
		
	} else {
		jsonParsed, err := gabs.ParseJSON(invokebody)
		if(err != nil) {
			log.Printf("DbSync Error parsing output: %s\n", err.Error())
		}
		value, ok := jsonParsed.Path("Errormessage").Data().(string)
		if(ok) {
			rr := fmt.Sprintf("Error in Invoke return: %s", value)
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

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.Datascope{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Printf("Unmarshaling error: %s\n", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.SaveDatascope(schema, t)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Datascope created"}
	} else {
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
	rq.Customer = schema
	rq.Datascope = &t.Name
	snc, _ := json.Marshal(rq)

	invokebody, err := authentication.Invoke("DbSync", nil, snc)
	if err != nil {
		log.Printf("DbSync Invoke Error: %s\n", err.Error())
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
	} else {
		log.Printf("DbSync success")
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.Printf("DbSync Error parsing output: %s\n", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if(ok) {
		rr := fmt.Sprintf("Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.Printf("%s\n", rr)
	} else {
		log.Printf("Invoke body: %s\n", string(invokebody))
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


func GetSelect(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeTable
log.Printf("input %s into %v\n", string(body ), t)

	err := json.Unmarshal(body, &t)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	ds, err := authentication.GetSelect(schema, &t)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	js, err := json.Marshal(ds)

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}	
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)

}

func GetDatascapeTables(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeId

	err := json.Unmarshal(body, &t)

	var status types.DatascopeTables
	ds, err := authentication.GetDatascopeTables(schema, t.Id)

	if(err != nil) {
		status = types.DatascopeTables{"Error", err.Error(), nil} 
	} else {
		status = types.DatascopeTables{"OK", "", ds} 
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

func GetDatascopeDetails(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.DatascopeId

	err := json.Unmarshal(body, &t)

	var status types.DatascopeInfoStatus
	ds, err := authentication.GetDatascope(schema, t.Id)
	if(err != nil) {
		status = types.DatascopeInfoStatus{"Error", err.Error(), nil} 
	} else {
		status = types.DatascopeInfoStatus{"OK", "", &ds} 
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
func UpdateConnection(w http.ResponseWriter, r *http.Request) {
	var status types.OperationStatus

	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.ConnectionRecord
	err := json.Unmarshal(body, &t)


	if err == nil {		
		conn, err := authentication.GetConnection(schema, *t.Id)

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
			log.Printf("DbAnalyzer Error: %s\n", err.Error())
			status = types.OperationStatus{"Error", err.Error()}
			
		} else {
			jsonParsed, err := gabs.ParseJSON(invokebody)
			if(err != nil) {
				log.Printf("DbSync Error parsing output: %s\n", err.Error())
			}
			value, ok := jsonParsed.Path("Errormessage").Data().(string)
			if(ok) {
				rr := fmt.Sprintf("Error in Invoke return: %s", value)
				status = types.OperationStatus{"Error", rr}
				log.Printf("%s\n", rr)
			} else {
				log.Printf("Invoke body: %s\n", string(invokebody))
			}
		}
		if(status.Status == "Error") {
			js, err := json.Marshal(status) //
			if err != nil {
				log.Printf("Error: %s\n", err.Error())
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
		status = types.OperationStatus{"Error", error.Error()}
	} else {
		status = types.OperationStatus{"OK", "Connection updated"}
	}
	js, err := json.Marshal(status) //
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func DeleteMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t struct {
		Id string
	}
	err := json.Unmarshal(body, &t)

	error := authentication.DeleteMapping(schema, t.Id)

	var status types.OperationStatus
	if(error != nil) {
		status = types.OperationStatus{"Error", error.Error()}
	} else {
		status = types.OperationStatus{"OK", "Mapping deleted"}
	}
	js, err := json.Marshal(status) //
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func UpdateMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupMapping
	err := json.Unmarshal(body, &t)

	error := authentication.UpdateMapping(schema, *t.Id, t.Dymiumgroup, t.Directorygroup, t.Comments, t.Adminaccess)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Connection created"}
	} else {
		status = types.OperationStatus{"Error", error.Error()}
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
func CreateMapping(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupMapping
	err := json.Unmarshal(body, &t)

	error := authentication.CreateNewMapping(schema, t.Dymiumgroup, t.Directorygroup, t.Comments)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Connection created"}
	} else {
		status = types.OperationStatus{"Error", error.Error()}
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
func CreateNewConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.ConnectionRecord
	err := json.Unmarshal(body, &t)

	id, error := authentication.CreateNewConnection(schema, t)
	var status types.OperationStatus
	if(error == nil) {
		status = types.OperationStatus{"OK", "Connection created"}
	} else {
		status = types.OperationStatus{"Error", error.Error()}
	}

	if(error == nil) {
		// get the connection details
		conn, err := authentication.GetConnection(schema, id)

		if err == nil {
			conn.TestOnly = true
			bconn, err := json.Marshal(conn)

			invokebody, err := authentication.Invoke("DbAnalyzer", nil, bconn)
			if err != nil {
				log.Printf("DbAnalyzer Error: %s\n", err.Error())
				status = types.OperationStatus{"Error", err.Error()}
				log.Println("return error")
			} else {
				jsonParsed, err := gabs.ParseJSON(invokebody)
				if(err != nil) {
					log.Printf("DbSync Error parsing output: %s\n", err.Error())
				}
				value, ok := jsonParsed.Path("Errormessage").Data().(string)
				if(ok) {
					rr := fmt.Sprintf("Error in Invoke return: %s", value)
					status = types.OperationStatus{"Error", rr}
					log.Printf("%s\n", rr)
				} else {
					log.Printf("Invoke body: %s\n", string(invokebody))
				}
			
			}
		}
	}
	if(status.Status == "Error") {
		authentication.DeleteConnection(schema, id)
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

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.Datascope{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Printf("Unmarshaling error: %s\n", err.Error() )
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	error := authentication.UpdateDatascope(schema, t)
	if error != nil {
		log.Printf("UpdateDatascope Error: %s\n", error.Error())
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
		log.Printf("DbSync Invoke Error: %s\n", err.Error())
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
	} else {
		log.Printf("DbSync success")
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.Printf("DbSync Error parsing output: %s\n", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	if(ok) {
		rr := fmt.Sprintf("Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.Printf("%s\n", rr)
	} else {
		status = types.OperationStatus{"OK", ""}
	
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

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := types.DatascopeIdName{}
	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Printf("Unmarshaling error: %s\n", err.Error() )
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
		log.Printf("DbSync Invoke Error: %s\n", err.Error())
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
	} else {
		log.Printf("DbSync success")
	}

	jsonParsed, err := gabs.ParseJSON(invokebody)
	if(err != nil) {
		log.Printf("DbSync Error parsing output: %s\n", err.Error())
	}
	value, ok := jsonParsed.Path("Errormessage").Data().(string)
	var js []byte
	if(ok) {
		rr := fmt.Sprintf("Error in Invoke return: %s", value)
		status = types.OperationStatus{"Error", rr}
		log.Printf("%s\n", rr)
	} else {
		status = types.OperationStatus{"OK", ""}
		error := authentication.DeleteDatascope(schema, t.Id)
		if error != nil {
			log.Printf("DeleteDatascope Error: %s\n", error.Error())
			status = types.OperationStatus{"Error", error.Error()}
		}
	}


	js, err = json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)		
}

func SaveGroups(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.GroupAssignment

	err := json.Unmarshal(body, &t)

	err = authentication.UpdateGroupAssignment(schema, t)

	var status  types.OperationStatus
	if err == nil {
		log.Printf("success")
		status = types.OperationStatus{"OK", "Groups Updated"}
	} else {
		log.Printf("Error %s\n", err.Error())
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
func DeleteConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	t := struct {
		Id string
	}{}

	err := json.Unmarshal(body, &t)

	err = authentication.DeleteConnection(schema, t.Id)
	var status  types.OperationStatus
	if err == nil {
		status = types.OperationStatus{"OK", "Connection deleted"}
	} else {
		log.Printf("Error: %s\n", err.Error())
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


func GetDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	datascopes, error := authentication.GetDatascopes(schema)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	if(error != nil) {
		status := types.DatascopesStatus{"Error", error.Error(), []types.DatascopeIdName{}}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	status := types.DatascopesStatus{"OK", "", datascopes}
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Write(js)
}
func GetMappings(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	mappings, error := authentication.GetMappings(schema)
	if(error != nil) {
		var status  types.GroupMappingStatus
		status = types.GroupMappingStatus{"Error", error.Error(), []types.GroupMapping{}}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	status := types.GroupMappingStatus{"OK", "", mappings}
	js, err := json.Marshal(status)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Write(js)
}

func GetGroupsForDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	
	mappings, error := authentication.GetGroupAssignments(schema)
	if(error != nil) {
		var status  types.OperationStatus
		status = types.OperationStatus{"Error", error.Error()}
		js, _ := json.Marshal(status)
		w.Write(js)
		return
	}
	js, err := json.Marshal(mappings)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Write(js)
}
func FakeLogin(w http.ResponseWriter, r *http.Request) {
	log.Printf("host: %s\n", r.Host)
	if(r.Host != "portal.dymium.local" || r.Host != os.Getenv("CUSTOMER_HOST")) {
		log.Printf("Error: fake login on non-local host requested")
		http.Error(w, "Fake login forbidden", http.StatusInternalServerError)
		return
	}
	js := authentication.GetFakeAuthentication()

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
	log.Printf("%s\n", logoutURL)

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


	token, name, groups, err :=  authentication.GetTunnelToken(t.Code, domain, clientid, clientsecret, redirecturl) 
	var ret types.AuthorizationCodeResponse
	ret.Token = token
	ret.Groups = groups 
	ret.Name = name
	
	js, err := json.Marshal(ret)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)

}
func GetDatascopesAccess(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email, groups, _ := authentication.GetIdentityFromToken(token)

	out, _ := authentication.GetDatascopesForGroups(schema, email, groups)

	js, _ := json.Marshal(out)

	log.Printf("In GetDatascopesAccess: %s, %v\n", email, groups)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)
}

func RegenerateDatascopePassword(w http.ResponseWriter, r *http.Request) {
	token := common.TokenFromHTTPRequest(r)
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	email, groups, _ := authentication.GetIdentityFromToken(token)

	out, _ := authentication.RegenerateDatascopePassword(schema, email, groups)

	js, _ := json.Marshal(out)

	log.Printf("In GetDatascopesAccess: %s, %v\n", email, groups)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Write(js)
}


func DownloadUpdate(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	os, _ := vars["os"]
	arch, _ := vars["arch"]	

	fil := fmt.Sprintf("./customer/update/%s/%s/tunnel", os, arch)
	log.Printf("path: %s\n", fil)
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
	fmt.Printf("schema=%s,t.Customerid=%s\n", schema, t.Customerid)
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

	fmt.Printf("Login URL: %s\n", resp.LoginURL)
	js, err := json.Marshal(resp)

	fmt.Printf("send back %s\n", string(js))
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

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
	log.Println("in GetClientCertificate")
	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()
	var t types.CertificateRequest
	log.Printf("body: %s\n", string(body))

	err := json.Unmarshal(body, &t)
	if err != nil {
		log.Printf("Error unmarshalling certificate request %s\n", err.Error())
	}

	log.Println("unmarshaled")
    
	pemBlock, _ := pem.Decode( []byte(t.Csr) )
    if pemBlock == nil {
		http.Error(w, "pem.Decode failed", http.StatusInternalServerError)
		return		
    }

	log.Println("Before ParseCertificateRequest")
	clientCSR, err := x509.ParseCertificateRequest(pemBlock.Bytes) 
    if err = clientCSR.CheckSignature(); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return 
    }

	log.Println("ParseCertificateRequest ready")
	// create client certificate template
    clientCRTTemplate := x509.Certificate{
        Signature:          clientCSR.Signature,
        SignatureAlgorithm: clientCSR.SignatureAlgorithm,

        PublicKeyAlgorithm: clientCSR.PublicKeyAlgorithm,
        PublicKey:          clientCSR.PublicKey,

        SerialNumber: big.NewInt(2),
        Issuer:       authentication.CaCert.Subject,
        Subject:      clientCSR.Subject,
        NotBefore:    time.Now().Add(-90 * time.Second), // grace time
        NotAfter:     time.Now().Add(90 * time.Second),
        KeyUsage:     x509.KeyUsageDigitalSignature,
        ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageClientAuth},
		DNSNames: clientCSR.DNSNames,
    }

	log.Printf("authentication.CaCert: %v\n", authentication.CaCert)
	log.Printf("authentication.CaKey: %v\n", authentication.CaKey)

    // create client certificate from template and CA public key
    clientCRTRaw, err := x509.CreateCertificate(rand.Reader, &clientCRTTemplate, authentication.CaCert, 
		clientCSR.PublicKey, authentication.CaKey)

	log.Println("past x509.CreateCertificate")		
    if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
    }


	log.Println("EncodeToMemory")	
    out := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: clientCRTRaw})
	log.Printf("certificate: %s\n", string(out))
	var certout types.CSRResponse 
	certout.Certificate = string(out)
	js, err := json.Marshal(certout)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(js))

}

func GetConnections(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")

	connections, error := authentication.GetConnections(schema)
	if(error != nil) {
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
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
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