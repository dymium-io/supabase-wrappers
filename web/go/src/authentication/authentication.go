/*
authentication
*/
package authentication

import (

	"database/sql"
	"net/http"
	"fmt"
	"log"
	"strconv"
	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
	"os"
	"encoding/json"
	"github.com/coreos/go-oidc/v3/oidc"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"time"
	_"golang.org/x/crypto/bcrypt"
	"dymium.com/dymium/common"
)

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
var auth_domain, auth_client_id, auth_client_secret, auth_callback, auth_redirect string
var ctx context.Context
var provider *oidc.Provider 
var config oauth2.Config

func DatabaseInit(host string, password string, port string) {
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
		panic(err)
	}
	err = db.Ping()
	if err != nil {
		panic(err)
	}

	auth_domain = os.Getenv("AUTH0_DOMAIN")
	auth_client_id = os.Getenv("AUTH0_CLIENT_ID")
	auth_client_secret = os.Getenv("AUTH0_CLIENT_SECRET")
	auth_callback = os.Getenv("AUTH0_CALLBACK_URL")
	auth_redirect = os.Getenv("AUTH0_REDIRECT_URL")

	ctx = context.Background()

	provider, err = oidc.NewProvider(ctx, auth_domain)
	if err != nil {
		log.Printf("Error creating provider: %s\n", err.Error())
	}
	config = oauth2.Config{
		ClientID:     auth_client_id,
		ClientSecret: auth_client_secret,
		Endpoint:     provider.Endpoint(),
		RedirectURL:  auth_callback,
		Scopes:       []string{oidc.ScopeOpenID, "profile", "email"},
	}
}

func generateJWT() (string, error) {
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

func AuthenticationHandlers(p *mux.Router) error {


	p.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {

		oauth2Token, err := config.Exchange(ctx, r.URL.Query().Get("code"))
		if err != nil {
			http.Error(w, "Failed to exchange token: "+err.Error(), http.StatusInternalServerError)
			return
		}

		userInfo, err := provider.UserInfo(ctx, oauth2.StaticTokenSource(oauth2Token))
		if err != nil {
			http.Error(w, "Failed to get userinfo: "+err.Error(), http.StatusInternalServerError)
			return
		}

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

		token, err := generateJWT()
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

	return nil
}

