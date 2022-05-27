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

		filename := "./borrower/static" + r.URL.Path
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

	b.HandleFunc("/api/createnewconnection", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
		if error != nil {
			log.Printf("Error: %s\n", error.Error())
			status := types.OperationStatus{"AuthError", error.Error()}
			js, err := json.Marshal(status)
			http.Error(w, err.Error(), http.StatusInternalServerError)
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

		var t types.Connection
		err := json.Unmarshal(body, &t)

		error = authentication.CreateNewConnection(schema, t)

		js, err := json.Marshal(t)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write(js)
	}).Methods("POST")

	b.HandleFunc("/api/getconnections", func(w http.ResponseWriter, r *http.Request) {
		token := common.TokenFromHTTPRequest(r)
		schema, error := authentication.GetSchemaFromToken(token)
		if error != nil {
			log.Printf("Error: %s\n", error.Error())
			status := types.OperationStatus{"AuthError", error.Error()}
			js, err := json.Marshal(status)
			http.Error(w, err.Error(), http.StatusInternalServerError)
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

	b.PathPrefix("/static").HandlerFunc(getImages)
	b.HandleFunc("/{name:.*\\.png}", getImages)
	b.HandleFunc("/{name:.*\\.gif}", getImages)
	b.HandleFunc("/{name:.*\\.svg}", getImages)
	b.HandleFunc("/{name:.*\\.jpg}", getImages)
	b.HandleFunc("/{name:.*\\.ico}", getImages)

	b.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		//commonheaders(w, r)
		fmt.Println("in /!\n")
		http.ServeFile(w, r, "./borrower/static/index.html")
	}).Methods("GET")
}
