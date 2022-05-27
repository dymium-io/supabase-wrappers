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
	"io"
	_"golang.org/x/crypto/bcrypt"
	"github.com/Jeffail/gabs"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
)



var psqlInfo string
var db *sql.DB

const timeOut = 20
var auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, 
	auth_admin_redirect, auth_admin_organization, auth_admin_audience string
var auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect, auth_portal_audience string
var ctx context.Context
var adminOIDCrovider *oidc.Provider 
var adminOauth2config oauth2.Config
var portalOIDCrovider *oidc.Provider 
var portalOauth2config oauth2.Config

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
	auth_admin_organization = os.Getenv("AUTH0_ADMIN_ORGANIZATION")
	auth_admin_audience = os.Getenv("AUTH0_ADMIN_AUDIENCE")

	ctx = context.Background()
	log.Printf("Create oidc provider: %s\n", auth_admin_domain)

	auth_portal_domain = os.Getenv("AUTH0_PORTAL_DOMAIN")
	auth_portal_client_id = os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	auth_portal_client_secret = os.Getenv("AUTH0_PORTAL_CLIENT_SECRET")
	auth_portal_redirect = os.Getenv("AUTH0_PORTAL_REDIRECT_URL")
	auth_portal_audience = os.Getenv("AUTH0_PORTAL_AUDIENCE")

	portalOIDCrovider, err = oidc.NewProvider(ctx, auth_portal_domain)

	if err != nil {
		log.Printf("Error creating provider: %s\n", err.Error())
	}
	portalOauth2config = oauth2.Config{
		ClientID:     auth_portal_client_id,
		ClientSecret: auth_portal_client_secret,
		Endpoint:     portalOIDCrovider.Endpoint(),
		RedirectURL:  auth_portal_redirect,
		Scopes:       []string{oidc.ScopeOpenID, "profile", "email"},
	}	
}

func generateAdminJWT(picture string) (string, error) {
		// generate JWT right header
		issueTime := time.Now()
		expirationTime := issueTime.Add(timeOut * time.Minute)
		
		claim := &types.AdminClaims{
			// TODO
			Roles: []string{},
			Picture: picture,
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
	claim := &types.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &types.Claims{
			Roles: []string{},
			Picture: claim.Picture,
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
func GetSchemaFromToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &types.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	if nil == err {
		if !tkn.Valid {
			err = errors.New("No error, but invalid token")
		}
	}
	if claim.Schema == "" {
		err = errors.New("Empty schema")
	}
	return claim.Schema, err

}
func CreateNewConnection(schema string, con types.Connection) error {
	sql := `insert into `+schema+`.connections(name, database_type, address, port, use_tls, description) 
		values($1,$2,$3,$4,$5,$6) returning id; `

	row := db.QueryRow(sql, con.Name, con.Dbtype, con.Address, con.Port, con.UseTLS, con.Description)
	var id string
	err := row.Scan(&id)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return err
	}
	// now let's save credentials!
	sql = `insert into `+schema+`.admincredentials(connection_id, username) values($1, $2) returning id`
	row = db.QueryRow(sql, id, con.Username)
	err = row.Scan(&id)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return err
	}
	sql = `insert into `+schema+`.passwords(id, password) values($1, $2)`;
	
	_, err = db.Exec(sql, id, con.Password)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return err
	}

	return nil
}
func GetConnections(schema string) ([]types.Connection, error ) {
	sql := `select a.id,a.name,a.address,a.port,a.database_type,a.use_tls,a.description,b.username,b.id from `+
		schema+`.connections as a join `+
		schema+`.admincredentials as b on a.id=b.connection_id;`
	rows, err := db.Query(sql)

	defer rows.Close()

	var conns = []types.Connection{}
	if nil == err {

		for rows.Next() {
			var conn = types.Connection{}
			err = rows.Scan(&conn.Id, &conn.Name, &conn.Address, &conn.Port, &conn.Dbtype, &conn.UseTLS, 
					&conn.Description, &conn.Username, &conn.Credid)
			if nil != err {
				log.Printf("Error in GetConnections:  %s\n", err.Error())
				return []types.Connection{}, err
			} else {
				conns = append(conns, conn)
			}
		}
		
	} else {
		log.Printf("Error in GetConnections:  %s\n", err.Error())
		return []types.Connection{}, nil
	}

	return conns, nil
}
func generatePortalJWT(picture, schema, org_id  string) (string, error) {
	// generate JWT right header
	issueTime := time.Now()
	expirationTime := issueTime.Add(timeOut * time.Minute)
	
	claim := &types.Claims{
		// TODO
		Roles: []string{},
		Picture: picture,
		Schema: schema,
		Orgid: org_id,
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
log.Printf("token: %s\n", tokenString)
	return tokenString, err
}

func refreshPortalToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &types.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &types.Claims{
			Roles: []string{},
			Picture: claim.Picture,
			Schema: claim.Schema,
			Orgid: claim.Orgid,			
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
func  getUserInfoFromToken(admin_domain, token string) ( []byte, error) {
	urlStr := fmt.Sprintf("%suserinfo", admin_domain)
	client := &http.Client{}

	nr, err := http.NewRequest(http.MethodGet, urlStr, nil) // URL-encoded payload
	if err != nil {
		log.Printf("Error: %s\n", err.Error()) 
		return []byte{}, err

	}

	nr.Header.Add("Authorization", "Bearer " + token)
	
	resp, err := client.Do(nr)

	if err != nil {
		log.Printf("Error: %s\n", err.Error()) 
		return []byte{}, err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)	
	
	return body, err
}
func getTokenFromCode(code, domain, client_id, client_secret, redirect string) ( []byte, error) {
	// get the token
	data := url.Values{}
	data.Set("grant_type", "authorization_code")
	data.Set("client_id", client_id)
	data.Set("client_secret", client_secret)
	data.Set("code", code)
	data.Set("redirect_uri", redirect)
	//data.Set("scope", "admin")
	encodedData := data.Encode()

	client := &http.Client{}
	urlStr := fmt.Sprintf("%soauth/token", 
	domain)
	log.Printf("in get token: %s\n%s\n", urlStr, encodedData)
	nr, err := http.NewRequest(http.MethodPost, urlStr, strings.NewReader(encodedData) ) // URL-encoded payload
	if err != nil {
		log.Printf("Error: %s\n", err.Error()) 
		return []byte{}, err

	}
	nr.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	resp, _ := client.Do(nr)
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)

	return body, err
}
func AuthenticationAdminHandlers(h *mux.Router) error {
	host := os.Getenv("ADMIN_HOST")
	p := h.Host(host).Subrouter()

	p.HandleFunc("/auth/refresh", func(w http.ResponseWriter, r *http.Request) {
		var status types.AuthStatus

		token := common.TokenFromHTTPRequest(r)

		newtoken, autherror := refreshAdminToken(token)
		if autherror == nil {
			status = types.AuthStatus{"OK", "Session live", newtoken}
		} else {
			log.Println("Return error, reauthenticate")
			status = types.AuthStatus{"Error", autherror.Error(), ""}
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
		code := r.URL.Query().Get("code")
		if code == "" {
			generateError(w, r, "Error: ", r.URL.Query().Get("error_description"))
			return
		} else {
			log.Printf("Returned code: %s\n", code)
		}

		body, err := getTokenFromCode(code, auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, auth_admin_redirect)

		log.Printf("returned body: %s\n", string(body))

		jsonParsed, err := gabs.ParseJSON(body)

		value, ok := jsonParsed.Path("error").Data().(string)
		if ok {
			log.Printf("Error: %s\n", value) 
			des, _ := jsonParsed.Path("error_description").Data().(string)
			generateError(w, r, value, des)
			return

		}
		access_token, ok := jsonParsed.Path("access_token").Data().(string)

		info, err := getUserInfoFromToken(auth_admin_domain, access_token)
		log.Printf("User info: %s\n", string(info))

		jsonParsed, err = gabs.ParseJSON(info)

		picture, ok := jsonParsed.Path("picture").Data().(string)
		token, err := generateAdminJWT(picture)
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
		org := r.URL.Query().Get("organization")
		var newquery string
		if org == "" {
			newquery = fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&organization=%s&audience=%s&scope=%s", 
				auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, url.QueryEscape(auth_admin_redirect),
				auth_admin_organization, url.QueryEscape(auth_admin_audience ),  url.QueryEscape("openid profile  email  groups permissions roles") )
		} else {
			newquery = fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&audience=%s&scope=%s", 
				auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, url.QueryEscape(auth_admin_redirect), url.QueryEscape(auth_admin_audience ),  url.QueryEscape("openid profile email  groups permissions roles") )

		}
		log.Printf("In /auth/login: redirect to \n%s\n", newquery)
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}

func AuthenticationPortalHandlers(h *mux.Router) error {
	host := os.Getenv("CUSTOMER_HOST")
	p := h.Host(host).Subrouter()

	p.HandleFunc("/auth/refresh", func(w http.ResponseWriter, r *http.Request) {
		var status types.AuthStatus

		token := common.TokenFromHTTPRequest(r)

		newtoken, autherror := refreshPortalToken(token)
		if autherror == nil {
			status = types.AuthStatus{"OK", "Session live", newtoken}
		} else {
			log.Println("Return error, reauthenticate")
			status = types.AuthStatus{"Error", autherror.Error(), ""}
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
		code := r.URL.Query().Get("code")
		if code == "" {
			generateError(w, r, "Error: ", r.URL.Query().Get("error_description"))
			return
		} else {
			log.Printf("Returned code: %s\n", code)
		}

		body, err := getTokenFromCode(code, auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect)

		log.Printf("returned body: %s\n", string(body))

		jsonParsed, err := gabs.ParseJSON(body)

		value, ok := jsonParsed.Path("error").Data().(string)
		if ok {
			log.Printf("Error: %s\n", value) 
			des, _ := jsonParsed.Path("error_description").Data().(string)
			generateError(w, r, value, des)
			return

		}
		access_token, ok := jsonParsed.Path("access_token").Data().(string)

		info, err := getUserInfoFromToken(auth_portal_domain, access_token)
		log.Printf("User info: %s\n", string(info))

		jsonParsed, err = gabs.ParseJSON(info)

		picture, ok := jsonParsed.Path("picture").Data().(string)
		log.Printf("picture: %s, ok: %b\n", picture, ok)

		org_id, ok := jsonParsed.Path("org_id").Data().(string)
		log.Printf("ord id: %s, ok: %b\n", org_id, ok)

		sql := fmt.Sprintf("select schema_name from global.customers where organization=$1;")

		row := db.QueryRow(sql, org_id)
		var schema string
		err = row.Scan(&schema)


		if(err != nil){
			log.Printf("Error: %s\n", err.Error() )
		} else{
			log.Printf("Schema: %s\n", schema )
		}

		token, err := generatePortalJWT(picture, schema, org_id)
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
		newquery := fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&scope=%s", 
			auth_portal_domain, r.URL.RawQuery, auth_portal_client_id, url.QueryEscape(auth_portal_redirect), url.QueryEscape("openid profile email groups permissions roles"))

		log.Printf("In /auth/login: redirect to \n%s\n", newquery)
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}
