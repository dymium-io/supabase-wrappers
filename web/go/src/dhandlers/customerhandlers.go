package dhandlers

import (
	"dymium.com/dymium/authentication"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	_"github.com/gorilla/mux"
	"golang.org/x/net/context"
	"encoding/json"
	"io/ioutil"
	"net/url"
	"errors"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"fmt"
	"aws"
)
type contextKey int
const authenticatedSchemaKey contextKey = 0


func AuthMiddleware(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// do stuff
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
		//create a new request context containing the authenticated user
		ctxWithSchema := context.WithValue(r.Context(), authenticatedSchemaKey, schema)
		//create a new request using that new context
		rWithSchema := r.WithContext(ctxWithSchema)

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
	// get the connection details
	log.Printf("%v\n", t)
	error := authentication.SaveDatascope(schema, t)


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
}
func GetDatascopeDetails(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

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
}
func UpdateConnection(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	body, _ := ioutil.ReadAll(r.Body)
	defer r.Body.Close()

	var t types.ConnectionRecord
	err := json.Unmarshal(body, &t)

	error := authentication.UpdateConnection(schema, t)
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

	error := authentication.UpdateMapping(schema, *t.Id, t.Dymiumgroup, t.Directorygroup, t.Comments)
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

	error := authentication.CreateNewConnection(schema, t)
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
func UpdateDatascope(w http.ResponseWriter, r *http.Request) {
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
}
func GetDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	datascopes, error := authentication.GetDatascopes(schema)

	js, err := json.Marshal(datascopes)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func GetMappings(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	mappings, error := authentication.GetMappings(schema)

	js, err := json.Marshal(mappings)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}

func GetGroupsForDatascopes(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	mappings, error := authentication.GetGroupAssignments(schema)

	js, err := json.Marshal(mappings)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
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

	logoutURL := fmt.Sprintf("%sv2/logout?returnTo=%s&client_id=%s",
		domain, url.QueryEscape(returnurl), clientid)
	log.Printf("%s\n", logoutURL)

	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	http.Redirect(w, r, logoutURL, 302)

}
func GetConnections(w http.ResponseWriter, r *http.Request) {
	schema := r.Context().Value(authenticatedSchemaKey).(string)

	connections, error := authentication.GetConnections(schema)

	js, err := json.Marshal(connections)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
	w.Write(js)
}
func GetImages(w http.ResponseWriter, r *http.Request) {
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
			Commonheaders(w, r)
		}
		http.ServeFile(w, r, filename)
	}
}