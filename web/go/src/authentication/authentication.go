/*
authentication
*/
package authentication

import (

	"database/sql"
	"net/http"
	"net/url"
	"fmt"
	"log"
	"strconv"
	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
	"os"
	"strings"
	"encoding/json"
	"github.com/coreos/go-oidc/v3/oidc"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"time"
	"errors"
	_"golang.org/x/crypto/bcrypt"
	"dymium.com/dymium/common"
)

type authStatus struct {
	Status string
	Text   string
	Token  string
}

type claims struct {
	ID         string   `json:"id"`
	Roles      []string `json:"roles"`
	CustomerId string   `json:"customerid"`
	jwt.StandardClaims
}

type adminClaims struct {
	Roles      []string `json:"roles"`
	jwt.StandardClaims
}

var psqlInfo string
var db *sql.DB

const timeOut = 20
var auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, auth_admin_redirect string
var ctx context.Context
var adminOIDCrovider *oidc.Provider 
var adminOauth2config oauth2.Config

func DatabaseInit(host string, password string, port string) {
	log.Println("In Database Init")
	nport := 5432
	if port != "" {
		nport, _ = strconv.Atoi(port)
	}
	psqlInfo = fmt.Sprintf("host=%s port=%d user=%s "+
		"password=%s dbname=%s sslmode=disable",
		host, nport, "dymium", password, "dymium")
	var err error
	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Printf("In Database, error: %s\n", err.Error())
		//panic(err)
	} else {
		log.Println("Database Connection opened")
		err = db.Ping()
		if err != nil {
			log.Printf("In Database, error: %s\n", err.Error())
			//panic(err)
		}		
	}


	auth_admin_domain = os.Getenv("AUTH0_ADMIN_DOMAIN")
	auth_admin_client_id = os.Getenv("AUTH0_ADMIN_CLIENT_ID")
	auth_admin_client_secret = os.Getenv("AUTH0_ADMIN_CLIENT_SECRET")
	auth_admin_redirect = os.Getenv("AUTH0_ADMIN_REDIRECT_URL")

	
	ctx = context.Background()
	log.Printf("Create oidc provider: %s\n", auth_admin_domain)
	adminOIDCrovider, err = oidc.NewProvider(ctx, auth_admin_domain)
	if err != nil {
		log.Printf("Error creating provider: %s\n", err.Error())
	}
	adminOauth2config = oauth2.Config{
		ClientID:     auth_admin_client_id,
		ClientSecret: auth_admin_client_secret,
		Endpoint:     adminOIDCrovider.Endpoint(),
		RedirectURL:  auth_admin_redirect,
		Scopes:       []string{oidc.ScopeOpenID, "profile", "email"},
	}
}
// this function extracts JWT from the request
func tokenFromAdminHTTPRequest(r *http.Request) string {
	reqToken := r.Header.Get("Authorization")
	var tokenString string
	splitToken := strings.Split(reqToken, "Bearer ")
	if len(splitToken) > 1 {
		tokenString = splitToken[1]
	}
	return tokenString
}
func generateAdminJWT() (string, error) {
		// generate JWT right header
		issueTime := time.Now()
		expirationTime := issueTime.Add(timeOut * time.Minute)
		
		claim := &adminClaims{
			// TODO
			Roles: []string{},
			StandardClaims: jwt.StandardClaims{
				// In JWT, the expiry time is expressed as unix milliseconds
				ExpiresAt: expirationTime.Unix(),
				IssuedAt: issueTime.Unix(),
			},
		}
		jwtKey := []byte(os.Getenv("SESSION_SECRET"))
		token := jwt.NewWithClaims(jwt.SigningMethodHS256, claim)
		// Create the JWT string
		tokenString, err := token.SignedString(jwtKey)

		return tokenString, err
}
func refreshAdminToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &claims{
			Roles: []string{},
			StandardClaims: jwt.StandardClaims{
				// In JWT, the expiry time is expressed as unix milliseconds
				ExpiresAt: expirationTime.Unix(),
				IssuedAt:  time.Now().Unix(),
			},
		}

		newtoken := jwt.NewWithClaims(jwt.SigningMethodHS256, newclaim)
		// Create the JWT string
		tokenString, err := newtoken.SignedString(jwtKey)

		if err != nil {
			log.Printf("Error: Database Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}

	// trigger redirect to authentication
	// log.Printf("Invalid session, expired at %v, time now: %v ", claim.StandardClaims.ExpiresAt, timeNow.Unix())
	return "", errors.New("Token invalid or expired")
}
func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	/*
	Failed to get userinfo: "+err.Error()
	*/
	log.Printf("Error %s: %s\n", header, body)
	w.Header().Set("Cache-Control", common.Nocache)
	w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(`<html>
		<head>
		<script>
		 !function() {
			window.location.href = "/app/error?header=`+url.QueryEscape(header)+`&body=`+url.QueryEscape(body)+`"
		 }()
		</script>
		</head>
		<body>Callback arrived</body>
		</html>`))

	return nil
}
func AuthenticationAdminHandlers(h *mux.Router) error {
	host := os.Getenv("ADMIN_HOST")
	p := h.Host(host).Subrouter()

	p.HandleFunc("/auth/refresh", func(w http.ResponseWriter, r *http.Request) {
		var status authStatus

		token := tokenFromAdminHTTPRequest(r)

		newtoken, autherror := refreshAdminToken(token)
		if autherror == nil {
			status = authStatus{"OK", "Session live", newtoken}
		} else {
			log.Println("Return error, reauthenticate")
			status = authStatus{"Error", autherror.Error(), ""}
		}

		js, err := json.Marshal(status)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "application/json")
		w.Header().Set("x-content-type-options", "nosniff")
		w.Header().Set("strict-transport-security", "max-age=31536000")
		w.Write(js)
	}).Methods("POST")

	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		error, _ := mux.Vars(r)["error"]
		error_description, _ := mux.Vars(r)["error_description"]		

		generateError(w, r, error, error_description)		
		return		
	}).
	Queries("error", "{error}").
    Queries("error_description", "{error_description}").Methods("GET")

	
	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {

		oauth2Token, err := adminOauth2config.Exchange(ctx, r.URL.Query().Get("code"))
		if err != nil {
			log.Printf("Error in adminOauth2config.Exchange %s\n", err.Error()) 
			generateError(w, r, "Failed to get exchange token", err.Error())		
			return
		}

		userInfo, err := adminOIDCrovider.UserInfo(ctx, oauth2.StaticTokenSource(oauth2Token))
		if err != nil {
			log.Printf("Error in adminOIDCrovider.UserInfo %s\n", err.Error()) 

			generateError(w, r, "Failed to get userinfo", err.Error())	
			return
		}
		log.Printf("User info: %s\n", userInfo)
		resp := struct {
			OAuth2Token *oauth2.Token
			UserInfo    *oidc.UserInfo
		}{oauth2Token, userInfo}

		// use data later
		_, err = json.MarshalIndent(resp, "", "    ")
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		
// save 

		token, err := generateAdminJWT()
		if(err != nil){
			log.Printf("Error: %s\n", err.Error() )
		}
		w.Header().Set("Cache-Control", common.Nocache)
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(`<html>
		<head>
		<script>
		 !function() {
			sessionStorage.setItem("Session", "`+token+`")
			window.location.href = "/app/"
		 }()
		</script>
		</head>
		<body>Callback arrived</body>
		</html>`))
		
	}).Methods("GET")

	p.HandleFunc("/auth/login", func(w http.ResponseWriter, r *http.Request) {
		newquery := fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&client_secret=%s", 
			auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, auth_admin_client_secret)

		log.Printf("In /auth/login: redirect to \n%s\n", newquery)
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}

