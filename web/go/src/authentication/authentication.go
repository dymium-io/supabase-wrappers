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
	"golang.org/x/net/context"
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
	log.Printf("%s\n", psqlInfo)
	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Printf("In Database, error: %s\n", err.Error())
		//panic(err)
	} else {
		log.Println("Database Connection opened successfully")
		err = db.Ping()
		if err != nil {
			log.Printf("In Database, error on Ping: %s\n", err.Error())
			//panic(err)
		}		
	}

	auth_admin_domain = os.Getenv("AUTH0_ADMIN_DOMAIN")
	auth_admin_client_id = os.Getenv("AUTH0_ADMIN_CLIENT_ID")
	auth_admin_client_secret = os.Getenv("AUTH0_ADMIN_CLIENT_SECRET")
	auth_admin_redirect = os.Getenv("AUTH0_ADMIN_REDIRECT_URL")
	auth_admin_organization = os.Getenv("AUTH0_ADMIN_ORGANIZATION")
	auth_admin_audience = os.Getenv("AUTH0_ADMIN_AUDIENCE")

	auth_portal_domain = os.Getenv("AUTH0_PORTAL_DOMAIN")
	auth_portal_client_id = os.Getenv("AUTH0_PORTAL_CLIENT_ID")
	auth_portal_client_secret = os.Getenv("AUTH0_PORTAL_CLIENT_SECRET")
	auth_portal_redirect = os.Getenv("AUTH0_PORTAL_REDIRECT_URL")
	auth_portal_audience = os.Getenv("AUTH0_PORTAL_AUDIENCE")

	// DELETE ME
	log.Printf("auth_portal_domain: %s\n", auth_portal_domain)
	log.Printf("auth_portal_client_id: %s\n", auth_portal_client_id)
	log.Printf("auth_portal_redirect: %s\n", auth_portal_redirect)	
	log.Printf("auth_portal_audience: %s\n", auth_portal_audience)	

	ctx = context.Background()

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
func CreateNewConnection(schema string, con types.ConnectionRecord) error {
	sql := `insert into `+schema+`.connections(name, database_type, address, port, dbname, use_tls, description) 
		values($1,$2,$3,$4,$5,$6,$7) returning id; `

	row := db.QueryRow(sql, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, con.Description)
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
func GetConnections(schema string) ([]types.ConnectionRecord, error ) {
	sql := `select a.id,a.name,a.address,a.port,a.dbname, a.database_type,a.use_tls,a.description,b.username,b.id from `+
		schema+`.connections as a join `+
		schema+`.admincredentials as b on a.id=b.connection_id;`
	rows, err := db.Query(sql)
	log.Printf("in GetConnections: %s\n", sql)
	defer rows.Close()

	var conns = []types.ConnectionRecord{}
	if nil == err {

		for rows.Next() {
			var conn = types.ConnectionRecord{}
			err = rows.Scan(&conn.Id, &conn.Name, &conn.Address, &conn.Port, &conn.Dbname, &conn.Dbtype, &conn.UseTLS, 
					&conn.Description, &conn.Username, &conn.Credid)
			if nil != err {
				log.Printf("Error in GetConnections:  %s\n", err.Error())
				return []types.ConnectionRecord{}, err
			} else {
				log.Printf("Connection: %v\n", conn)
				conns = append(conns, conn)
			}
		}
		
	} else {
		log.Printf("Error in GetConnections:  %s\n", err.Error())
		return conns, err
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

func UpdateConnection(schema string, con types.ConnectionRecord) error {
	// Create a new context, and begin a transaction
	ctx := context.Background()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in UpdateConnection: %s\n", err.Error())
		return err
	}
	sql := `update `+schema+`.connections set name=$1, database_type=$2, 
		address=$3, port=$4, dbname=$5, use_tls=$6, description=$7 where id=$8  
		and exists (select schema_name from global.customers where schema_name = $9);; `

	_, err  = tx.ExecContext(ctx, sql, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, con.Description, con.Id, schema)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in UpdateConnection: %s\n", err.Error())
		return err
	}
	if(con.Username != "" && con.Password != "") {
		sql = `update `+schema+`.admincredentials set username=$1 where connection_id=$2 returning id;`
		row  := tx.QueryRowContext(ctx, sql, con.Username, con.Id)
		var credid string
		err = row.Scan(&credid)
		if err != nil {
			tx.Rollback()
			log.Printf("Error 2 in UpdateConnection: %s\n", err.Error())
			return err
		}
		sql = `update `+schema+`.passwords set password=$1 where id=$2;`
		_, err  = tx.ExecContext(ctx, sql, con.Password, credid)
		if err != nil {
			tx.Rollback()
			log.Printf("Error 3 in UpdateConnection: %s\n", err.Error())
			return err
		}		
	}
	log.Printf("Username: %s, password %s\n", con.Username, con.Password )
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		return err
	}
	log.Printf("Returning success")	
	return nil
}
func GetConnection(schema, id string) (types.Connection, error) {
	sql := `select a.database_type,a.address,a.port,b.username,c.password,a.dbname, a.use_tls from 
		spoofcorp.connections as a join spoofcorp.admincredentials as b on a.id=b.connection_id 
			join spoofcorp.passwords as c on b.id=c.id where a.id=$1;`
	row := db.QueryRow(sql, id)
	var con types.Connection
	err := row.Scan(&con.Typ, &con.Address, &con.Port, &con.User, &con.Password, &con.Database, &con.Tls)
	if(err != nil) {
		log.Printf("Error: ", err.Error())
	} else {
		log.Printf("connection: %v", con)
	}
	return con, err

}
func SaveDatascope(schema string, dscope types.Datascope) error {
	// Create a new context, and begin a transaction
	ctx := context.Background()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in SaveDatascope: %s\n", err.Error())
		return err
	}

	// check if datascope exists, validating the schema along the way
	sql := "select count(id) from "+schema+".datascopes where name=$1 and exists (select schema_name from global.customers where schema_name = $2);"
	row := tx.QueryRowContext(ctx, sql, dscope.Name, schema)
	var count int
	err = row.Scan(&count)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in SaveDatascope: %s\n", err.Error())
		return err
	}	
	if(count != 0) {
		tx.Rollback()
		log.Printf("count: %d\n", count)
		err := errors.New("A record for datascope '" + dscope.Name + "' already exists!")
		log.Printf("Error 2 in SaveDatascope: %s\n", err.Error())
		return err
	}
	sql="insert into "+schema+".datascopes(name) values($1)  returning id;"
	row = tx.QueryRowContext(ctx, sql, dscope.Name)
	var ds_id string
	err = row.Scan(&ds_id)	
	if err != nil {
		tx.Rollback()
		log.Printf("Error 3 in SaveDatascope: %s\n", err.Error())
		return err
	}	
	log.Printf("id=%s\n", ds_id)
	// iterate and create 
	records := dscope.Records
	for  _, r := range  records  {
		if(r.Reference == nil) {
			sql=`insert into ` + schema + `.tables(datascope_id, col, connection_id, schem, tabl, typ, reference, semantics, action, position) 
			values($1, $2, (select id from ` + schema + `.connections where name=$3), $4, $5, $6, $7, $8, $9, $10);`
			
			_, err = tx.ExecContext(ctx, sql, ds_id, r.Col, r.Connection, r.Schema, r.Table, r.Typ, r.Reference, r.Semantics, r.Action, r.Position)
			if err != nil {
				tx.Rollback()
				log.Printf("Error 4 in SaveDatascope record: %s\n", err.Error())
				return err
			}	
		} else {
			log.Printf("schema: %s, table: %s, col:%s\n", r.Reference.Schema, r.Reference.Table, r.Reference.Column)
			sql=`with rows as (insert into ` + schema + `.refs(schem, tabl, col) values($1, $2, $3) returning id) insert into ` + schema + 
			`.tables(datascope_id, col, connection_id, schem, tabl, typ, semantics, action, position, reference) 
			values($4, $5, (select id from ` + schema + `.connections where name=$6), $7, $8, $9, 
			 $10, $11, $12, (select id from rows));`
			log.Printf("%s\n", sql)
			_, err = tx.ExecContext(ctx, sql, 
				r.Reference.Schema, r.Reference.Table, r.Reference.Column,
				ds_id, r.Col, r.Connection, r.Schema, r.Table, r.Typ, 
				r.Semantics, r.Action, r.Position)
				
			if err != nil {
				tx.Rollback()
				log.Printf("Error 5 in SaveDatascope record: %s\n", err.Error())
				return err
			}				
		}
	
	}



	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 5 in SaveDatascope: %s\n", err.Error())
		return err
	}	
	log.Println("SaveDatascope success!")
	return nil
}
func DeleteConnection(schema, id string) error {
	// Create a new context, and begin a transaction
	ctx := context.Background()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in DeleteConnection: %s\n", err.Error())
		return err
	}
	sql := "select id from " + schema + ".admincredentials where connection_id=$1 and exists (select schema_name from global.customers where schema_name = $2);";
	log.Printf("sql: %s\n", sql)
	row := tx.QueryRowContext(ctx, sql, id, schema)
	var credid string
	err = row.Scan(&credid)
	if err != nil {
		log.Printf("Error 2 in DeleteConnection: %s\n", err.Error())
		return err
	}
	sql = "delete from "+schema+".passwords where id=$1;"
	_, err = tx.ExecContext(ctx, sql, credid)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 3 in DeleteConnection: %s\n", err.Error())
		return err
	}
	sql = "delete from "+schema+".admincredentials where id=$1;"
	_, err = tx.ExecContext(ctx, sql, credid)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 4 in DeleteConnection: %s\n", err.Error())
		return err
	}
	sql = "delete from "+schema+".connections where id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 5 in DeleteConnection: %s\n", err.Error())
		return err
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 6 in DeleteConnection: %s\n", err.Error())
		return err
	}
	log.Printf("Returning success")
	return nil
}
func AuthenticationAdminHandlers(h *mux.Router) error {
	host := os.Getenv("ADMIN_HOST")
	log.Printf("ADMIN_HOST: %s\n", host)	
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
				auth_admin_organization, url.QueryEscape(auth_admin_audience ),  url.QueryEscape("groups") )
		} else {
			newquery = fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&audience=%s&scope=%s", 
				auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, url.QueryEscape(auth_admin_redirect), url.QueryEscape(auth_admin_audience ),  url.QueryEscape("groups") )

		}
		log.Printf("In /auth/login: redirect to \n%s\n", newquery)
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}

func AuthenticationPortalHandlers(h *mux.Router) error {
	host := os.Getenv("CUSTOMER_HOST")
	log.Printf("CUSTOMER_HOST: %s\n", host)
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
