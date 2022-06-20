package customer

import (
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/gorilla/mux"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
	"aws"
)

func CustomerHandlers(p *mux.Router) {
	host := os.Getenv("CUSTOMER_HOST")
	b := p.Host(host).Subrouter()

	commonheaders := func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", common.Cachedirective)
		w.Header().Set("x-content-type-options", "nosniff")
		w.Header().Set("strict-transport-security", "max-age=31536000")
	}

	getImages := func(w http.ResponseWriter, r *http.Request) {

		filename := "./customer/" + r.URL.Path
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

	b.HandleFunc("/api/queryconnection", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()
		t := struct {
			ConnectionId string
		}{}
		err := json.Unmarshal(body, &t)

		// get the connection details
		conn, err := authentication.GetConnection(schema, t.ConnectionId)
		bconn, err := json.Marshal(conn)

        res, err := aws.Invoke("DbAnalyzer", nil, bconn)
		if err != nil {
			log.Printf("DbAnalyzer Error: ", err.Error())
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		} else {
			log.Printf("DbAnalyzer success")
		}


		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(res)
	}).Methods("POST")

	b.HandleFunc("/api/savedatascope", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		t := types.Datascope{}
		err := json.Unmarshal(body, &t)
		if err != nil {
			log.Printf("Unmarshaling error: %s\n", err.Error() )
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		// get the connection details
		log.Printf("%v\n", t)
		error = authentication.SaveDatascope(schema, t)


		var rq types.Request
		rq.Action = types.A_Update
		rq.Customer = schema
		rq.Datascope = &t.Name
		snc, _ := json.Marshal(rq)

		_, err = aws.Invoke("DbSync", nil, snc)
		if err != nil {
			log.Printf("DbSync Error: ", err.Error())
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		} else {
			log.Printf("DbSync success")
		}

		var status types.OperationStatus
		if(error == nil) {
			status = types.OperationStatus{"OK", "Datascope created"}
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
	}).Methods("POST")	
	

	b.HandleFunc("/api/updatedatascope", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		t := types.Datascope{}
		err := json.Unmarshal(body, &t)
		if err != nil {
			log.Printf("Unmarshaling error: %s\n", err.Error() )
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		// get the connection details
		log.Printf("%v\n", t)
		error = authentication.UpdateDatascope(schema, t)


		var rq types.Request
		rq.Action = types.A_Update
		if( len(t.Records) == 0) {
			rq.Action = types.A_Delete
		}

		rq.Customer = schema
		rq.Datascope = &t.Name
		snc, _ := json.Marshal(rq)

		_, err = aws.Invoke("DbSync", nil, snc)
		if err != nil {
			log.Printf("DbSync Error: ", err.Error())
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		} else {
			log.Printf("DbSync success")
		}

		var status types.OperationStatus
		if(error == nil) {
			status = types.OperationStatus{"OK", "Datascope created"}
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
	}).Methods("POST")

	b.HandleFunc("/api/createnewconnection", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		var t types.ConnectionRecord
		err := json.Unmarshal(body, &t)

		error = authentication.CreateNewConnection(schema, t)
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
	}).Methods("POST")

	b.HandleFunc("/api/createmapping", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		var t types.GroupMapping
		err := json.Unmarshal(body, &t)

		error = authentication.CreateNewMapping(schema, t.Dymiumgroup, t.Directorygroup, t.Comments)
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
	}).Methods("POST")

	b.HandleFunc("/api/updatemapping", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		var t types.GroupMapping
		err := json.Unmarshal(body, &t)

		error = authentication.UpdateMapping(schema, *t.Id, t.Dymiumgroup, t.Directorygroup, t.Comments)
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
	}).Methods("POST")
	
	b.HandleFunc("/api/deletemapping", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		var t struct {
			Id string
		}
		err := json.Unmarshal(body, &t)

		error = authentication.DeleteMapping(schema, t.Id)

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
	}).Methods("POST")

	b.HandleFunc("/api/updateconnection", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		var t types.ConnectionRecord
		err := json.Unmarshal(body, &t)

		error = authentication.UpdateConnection(schema, t)
		var status types.OperationStatus
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
	}).Methods("POST")

	b.HandleFunc("/api/getdatascopedetails", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		t := struct {
			Id string
		}{}

		err := json.Unmarshal(body, &t)

		ds, err := authentication.GetDatascope(schema, t.Id)
		js, err := json.Marshal(ds)

		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("POST")
	
	b.HandleFunc("/api/savegroups", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
	}).Methods("POST")

	b.HandleFunc("/api/deleteconnection", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		t := struct {
			Id string
		}{}

		err := json.Unmarshal(body, &t)

		err = authentication.DeleteConnection(schema, t.Id)
		var status  types.OperationStatus
		if err == nil {
			log.Printf("success")
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
	}).Methods("POST")

	b.HandleFunc("/api/getgroupsfordatascopes", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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

		mappings, error := authentication.GetGroupAssignments(schema)

		js, err := json.Marshal(mappings)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("GET")

	 b.HandleFunc("/api/getmappings", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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

		mappings, error := authentication.GetMappings(schema)

		js, err := json.Marshal(mappings)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("GET")

	b.HandleFunc("/api/getdatascopes", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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

		datascopes, error := authentication.GetDatascopes(schema)

		js, err := json.Marshal(datascopes)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("GET")

	b.HandleFunc("/api/getconnections", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
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

		connections, error := authentication.GetConnections(schema)

		js, err := json.Marshal(connections)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("GET")

	b.HandleFunc("/api/getlogin", func(w http.ResponseWriter, r *http.Request) {
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
	}).Methods("GET")

	b.HandleFunc("/api/logout", func(w http.ResponseWriter, r *http.Request) {
		domain := os.Getenv("AUTH0_PORTAL_DOMAIN")
		clientid := os.Getenv("AUTH0_PORTAL_CLIENT_ID")
		returnurl := os.Getenv("AUTH0_PORTAL_RETURN_URL")

		logoutURL := fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s",
			domain, url.QueryEscape(returnurl), clientid)
		log.Printf("%s\n", logoutURL)

		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		http.Redirect(w, r, logoutURL, 302)

	}).Methods("GET")

	// For React to work properly, ensure that the URLs going into the React router return index.html
	b.PathPrefix("/app/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./customer/index.html")
	})
	b.PathPrefix("/services/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./customer/index.html")
	})
	b.PathPrefix("/resources/").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		commonheaders(w, r)
		http.ServeFile(w, r, "./customer/index.html")
	})

	b.PathPrefix("/static").HandlerFunc(getImages)
	b.HandleFunc("/{name:.*\\.png}", getImages)
	b.HandleFunc("/{name:.*\\.gif}", getImages)
	b.HandleFunc("/{name:.*\\.svg}", getImages)
	b.HandleFunc("/{name:.*\\.jpg}", getImages)
	b.HandleFunc("/{name:.*\\.ico}", getImages)

	b.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		//commonheaders(w, r)
		fmt.Println("in /!\n")
		http.ServeFile(w, r, "./customer/index.html")
	}).Methods("GET")
}
