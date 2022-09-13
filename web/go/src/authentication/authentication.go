//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
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
	"github.com/lib/pq"
	"encoding/pem"	
	"crypto/x509"
	"os"
	"strings"
	"encoding/json"
	"golang.org/x/net/context"
	"time"
	"errors"
	"io"
	"crypto/rsa"
	"github.com/Jeffail/gabs"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	"path/filepath"
	"aws"
	"crypto/rand"
	"math/big"
)



var psqlInfo string
var db *sql.DB
var DefaultPort = 24354
const timeOut = 20
var auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, 
	auth_admin_redirect, auth_admin_organization, auth_admin_audience string
var auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect, auth_portal_audience string
var ctx context.Context

var FilesystemRoot string

var Invoke types.Invoke_t

var CaKey *rsa.PrivateKey
var CaCert *x509.Certificate 

var stdChars = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$^&*()-_=+,.?:;{}[]")

func generatePassword(passwordLength int) string {
	pword := make([]byte, passwordLength)
	length := len(stdChars)

	getChar := func() byte {
		j, err := rand.Int(rand.Reader, big.NewInt(int64(length)))
		if err != nil {
			log.Printf("Error generating crypto strong int")
		}
		return stdChars[ int(j.Int64()) ]
	}
	for i := 0; i < passwordLength; i++ {
		pword[i] = getChar()
	}
	return string(pword)
}

func ParsePEMPrivateKey(pemBytes []byte, passphrase string) (*rsa.PrivateKey, error) {
	block, _ := pem.Decode(pemBytes)
	if block == nil {
		return nil, errors.New("no valid private key found")
	}

	switch block.Type {
	case "RSA PRIVATE KEY":
		var privKeyBytes []byte
		var err error

		if x509.IsEncryptedPEMBlock(block) {
			privKeyBytes, err = x509.DecryptPEMBlock(block, []byte(passphrase))
			if err != nil {
				return nil, errors.New("could not decrypt private key")
			}
		} else {
			privKeyBytes = block.Bytes
		}

		rsaPrivKey, err := x509.ParsePKCS1PrivateKey(privKeyBytes)
		if err != nil {
			return nil, fmt.Errorf("could not parse DER encoded key: %v", err)
		}

		return rsaPrivKey, nil

	default:
		return nil, fmt.Errorf("unsupported key type %q", block.Type)
	}
}
func InitInvoke( i types.Invoke_t) {
	Invoke = i
}
//aws.Invoke("DbAnalyzer", nil, bconn)
func Init(host string, port, user, password, dbname, tls string) error {
	nport := 5432
	if port != "" {
		nport, _ = strconv.Atoi(port)
	}
	psqlInfo = fmt.Sprintf("host=%s port=%d user=%s "+
		"dbname=%s sslmode=%s",
		host, nport, user, dbname, tls)
        if password != "" {
		psqlInfo += " password='"+password+"'"
        }
	var err error

	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Printf("In Database, error: %s\n", err.Error())
		return err
	} else {
		err = db.Ping()
		if err != nil {
			log.Printf("In Database, error on Ping: %s\n", err.Error())
			return err
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

	ctx = context.Background()

	FilesystemRoot = os.Getenv("FILESYSTEM_ROOT")
	if(FilesystemRoot == "") {
		FilesystemRoot, _ = os.Getwd()
	} else {
		FilesystemRoot, _ = filepath.Abs(FilesystemRoot)
	}
	Invoke = aws.Invoke

	ca_cert := os.Getenv("CA_AUTHORITY")
	t := struct {
		Key string
		Certificate string
	}{}	
	err = json.Unmarshal([]byte(ca_cert), &t)
	if err != nil {
		log.Printf("Cert unmarshaling error: %s\n", err.Error() )
		return nil
	}
	
	pemcert, _ := pem.Decode([]byte(t.Certificate))

	CaCert, err = x509.ParseCertificate(pemcert.Bytes)
	if err != nil {
		log.Printf("Cert parsing error: %s\n", err.Error() )
		return nil
	}

	passphrase := os.Getenv("CA_PASSPHRASE")
	CaKey, err = ParsePEMPrivateKey([]byte(t.Key), passphrase)

	if err != nil {
		log.Printf("Key parsing error: %s\n", err.Error() )
		return nil
	}

	return nil
}

func GetDB() *sql.DB {
	return db
}
func generateAdminJWT(picture string, name string, email string, groups []string, org_id string) (string, error) {
		// generate JWT right header
		issueTime := time.Now()
		expirationTime := issueTime.Add(timeOut * time.Minute)
		
		claim := &types.AdminClaims{
			// TODO
			Name: name,
			Email: email,
			Groups: groups,
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
	claim := &types.AdminClaims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &types.AdminClaims{
			Name: claim.Name,
			Email: claim.Email,
			Groups: claim.Groups,
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

func UsernameFromEmail(email string) string {
	username := strings.Split(email, "@")[0]
	fmt.Printf("username: %s\n", username)
//!#$%&'*+-/=?^_`{|}~
	replacer := strings.NewReplacer(
		"!", "_", 
		"#", "_", 
		"$", "_", 
		"%", "_", 
		"&", "_", 
		"'", "_", 
		"*", "_", 
		"+", "_", 
		"-", "_",
		"/", "=", 
		"?", "_", 
		"^", "_",
		"`", "_", 
		"{", "_", 
		"|", "_", 
		"}", "_", 
		"~", "_")

	username = replacer.Replace(username)
	fmt.Printf("username: %s\n", username)
	return username
}


func RegenerateDatascopePassword(schema string, email string, groups []string) (types.UserDatascopes, error) {
	var out types.UserDatascopes
	var conf types.UserConf

	var rq types.Request
	rq.Action = types.A_ConfUser
	rq.Customer = schema
	rq.UserConf = &conf

	username := UsernameFromEmail(email)
	password := generatePassword(10) 

	rq.UserConf.Name = username
	rq.UserConf.Password = password

	sqlName := `update ` + schema + `.users set password=$1, lastchanged=now() where username=$2;`
	_, err := db.Exec(sqlName, password, username)
	if(err != nil) {
		log.Printf("Error: %s\n", err.Error())
	}

	sql := `select distinct a.name, a.id from `+schema+`.datascopes as a  join `+schema+`.groupsfordatascopes as b on a.id=b.datascope_id join `+schema+`.groupmapping as c on c.id=b.group_id where c.outergroup = any ($1)`

    out.Username = username
	out.Password = password
	out.Schema = schema
	rows, err := db.Query(sql, pq.Array(groups))
	if nil == err {
		defer rows.Close()
		log.Println ("No error")
		for rows.Next() {
			var ds types.DatascopeIdName
			err = rows.Scan(&ds.Name, &ds.Id) 
			if err != nil {
				log.Printf("Error: %s\n", err.Error())
			} else {
				out.Datascopes = append(out.Datascopes, ds)
				
			}
			rq.UserConf.Datascopes = append(rq.UserConf.Datascopes, ds.Name)
			
		}
	} else { 
		log.Printf("Error: %s\n", err.Error())
	}



	snc, _ := json.Marshal(rq)	
	_, err = Invoke("DbSync", nil, snc)
	if(err != nil) {
		// TODO - pass this error
		fmt.Printf("Error syncing datascopes: %s\n", err.Error())
	}
	return out, nil
}

func GetDatascopesForGroups(schema string, email string, groups []string) (types.UserDatascopes, error) {
	var out types.UserDatascopes
	var conf types.UserConf
	var rq types.Request
	rq.Action = types.A_ConfUser
	rq.Customer = schema
	rq.UserConf = &conf

	username := UsernameFromEmail(email)
	var password string
	var age float32
	sqlName := `select password, EXTRACT(epoch from (now() - lastchanged)) from ` + schema + `.users where username=$1;`
	row := db.QueryRow(sqlName, username)
	err := row.Scan(&password, &age)


	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		password = generatePassword(10) 
		sqlName := `insert into ` + schema + `.users (username,password)  values($1, $2);`
		_, err = db.Exec(sqlName, username, password)
		log.Printf("sqlName: %s\n", sqlName)
		if(err != nil) {
			log.Printf("Error: %s\n", err.Error())
		}
	}
	sql := `select distinct a.name, a.id from `+schema+`.datascopes as a  join `+schema+`.groupsfordatascopes as b on a.id=b.datascope_id join `+schema+`.groupmapping as c on c.id=b.group_id where c.outergroup = any ($1)`

	if age >  60*60*24 {
		password = `**********`
	}
    out.Username = username
	out.Password = password
	out.Schema = schema

	rq.UserConf.Name = username
	rq.UserConf.Password = password

	rows, err := db.Query(sql, pq.Array(groups))
	if nil == err {
		defer rows.Close()
		log.Println ("No error")
		for rows.Next() {
			var ds types.DatascopeIdName
			err = rows.Scan(&ds.Name, &ds.Id) 
			if err != nil {
				log.Printf("Error: %s\n", err.Error())
			} else {
				out.Datascopes = append(out.Datascopes, ds)
				
			}
			rq.UserConf.Datascopes = append(rq.UserConf.Datascopes, ds.Name)			
		}
	} else { 
		log.Printf("Error: %s\n", err.Error())
	}


	snc, _ := json.Marshal(rq)	
	_, err = Invoke("DbSync", nil, snc)

	if(err != nil) {
		// TODO - pass this error
		fmt.Printf("Error syncing datascopes: %s\n", err.Error())
	}
	return out, nil
}

func GetIdentityFromToken(token string) (string, []string, error) {
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
	return claim.Email, claim.Groups, err
}

func CreateNewConnection(schema string, con types.ConnectionRecord) (string, error) {
	sql := `insert into `+schema+`.connections(name, database_type, address, port, dbname, use_tls, description) 
		values($1,$2,$3,$4,$5,$6,$7) returning id; `

	row := db.QueryRow(sql, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, con.Description)
	var id string
	err := row.Scan(&id)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return "", err
	}
	// now let's save credentials!
	var credid string
	sql = `insert into `+schema+`.admincredentials(connection_id, username) values($1, $2) returning id`
	row = db.QueryRow(sql, id, con.Username)
	err = row.Scan(&credid)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return id, err
	}
	sql = `insert into `+schema+`.passwords(id, password) values($1, $2)`;
	
	_, err = db.Exec(sql, credid, con.Password)
	if err != nil {
		log.Printf("Error: %s\n", err.Error())
		return id, err
	}

	return id, nil
}
func GetConnections(schema string) ([]types.ConnectionRecord, error ) {
	sql := `select a.id,a.name,a.address,a.port,a.dbname, a.database_type,a.use_tls,a.description,b.username,b.id from `+
		schema+`.connections as a join `+
		schema+`.admincredentials as b on a.id=b.connection_id;`
	rows, err := db.Query(sql)

	var conns = []types.ConnectionRecord{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var conn = types.ConnectionRecord{}
			err = rows.Scan(&conn.Id, &conn.Name, &conn.Address, &conn.Port, &conn.Dbname, &conn.Dbtype, &conn.UseTLS, 
					&conn.Description, &conn.Username, &conn.Credid)
			if nil != err {
				log.Printf("Error in GetConnections:  %s\n", err.Error())
				return []types.ConnectionRecord{}, err
			} else {
				conns = append(conns, conn)
			}
		}
	} else {
		log.Printf("Error in GetConnections:  %s\n", err.Error())
		return conns, err
	}

	return conns, nil
}

func  CheckAndRefreshToken(token string, sport string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &types.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	nport, _ := strconv.Atoi(sport)

	if err == nil && tkn.Valid {
		// update token
		log.Printf("claim: %v, groups:%v\n", claim, claim.Groups)

		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &types.Claims{
			Name: claim.Name,
			Email: claim.Email,
			Groups: claim.Groups,
			Picture: claim.Picture,
			Schema: claim.Schema ,
			Port: nport,
			Orgid: claim.Orgid ,	
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
			log.Printf("Error: Token Signing Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}	

// trigger redirect to authentication
// log.Printf("Invalid session, expired at %v, time now: %v ", claim.StandardClaims.ExpiresAt, timeNow.Unix())
return "", err
}

func GeneratePortalJWT(picture string, schema string, name string, email string, groups []string, org_id  string) (string, error) {
	// generate JWT right header
	issueTime := time.Now()
	expirationTime := issueTime.Add(timeOut * time.Minute)

	claim := &types.Claims{
		// TODO
		Name: name,
		Groups: groups,
		Email: email,
		Picture: picture ,
		Schema: schema,
		Port: DefaultPort,
		Orgid: org_id,
		StandardClaims: jwt.StandardClaims{
			// In JWT, the expiry time is expressed as unix milliseconds
			ExpiresAt: expirationTime.Unix(),
			IssuedAt: issueTime.Unix(),
		},
	}
	log.Printf("claim: %v, groups:%v\n", claim, groups)
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claim)
	// Create the JWT string
	tokenString, err := token.SignedString(jwtKey)

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
		log.Printf("claim: %v, groups:%v\n", claim, claim.Groups)

		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &types.Claims{
			Name: claim.Name,
			Email: claim.Email,
			Groups: claim.Groups,
			Picture: claim.Picture,
			Schema: claim.Schema ,
			Port: claim.Port,
			Orgid: claim.Orgid ,	
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
			log.Printf("Error: Token Signing Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}

	// trigger redirect to authentication
	// log.Printf("Invalid session, expired at %v, time now: %v ", claim.StandardClaims.ExpiresAt, timeNow.Unix())
	return "", err
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
func  getUserInfoFromToken(admin_domain, token, id_token string) ( string, string, string, []string, string, error) {
	urlStr := fmt.Sprintf("%suserinfo", admin_domain)
	client := &http.Client{}

	nr, err := http.NewRequest(http.MethodGet, urlStr, nil) // URL-encoded payload
	if err != nil {
		log.Printf("Error: %s\n", err.Error()) 
		return "", "", "", []string{}, "", err

	}
	nr.Header.Add("Authorization", "Bearer " + token)
	
	resp, err := client.Do(nr)

	if err != nil {
		log.Printf("Error: %s\n", err.Error()) 
		return "", "", "", []string{}, "", err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)	
log.Printf("%s\n", string(body))
	jsonParsed, err := gabs.ParseJSON(body)
	org_id, _ := jsonParsed.Path("org_id").Data().(string)
	name, _ := jsonParsed.Path("name").Data().(string)
	email, _ :=  jsonParsed.Path("preferred_username").Data().(string)
	picture, _ := jsonParsed.Path("picture").Data().(string)
	var groups []string

	mp, err := jsonParsed.ChildrenMap()

	dymium, _ := mp["https://dymium/"]

	gr, err := dymium.Path("groups").Children()
	for _, child := range  gr {
		groups = append(groups, child.Data().(string))
	}
log.Printf("groups: %v\n", groups)
	return picture, name, email, groups, org_id, err
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
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in UpdateConnection: %s\n", err.Error())
		return err
	}
	sql := `update `+schema+`.connections set name=$1, database_type=$2, 
		address=$3, port=$4, dbname=$5, use_tls=$6, description=$7 where id=$8  
		and exists (select schema_name from global.customers where schema_name = $9) returning id; `

	row  := tx.QueryRowContext(ctx, sql, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, con.Description, con.Id, schema)
	var newid string
	err = row.Scan(&newid)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in UpdateConnection: %s\n", err.Error())
		return err
	}
	if(con.Username != nil && con.Password != nil) {
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
	if(con.Username != nil && con.Password != nil) {
		log.Printf("Username: %s, password %s\n", *con.Username, *con.Password )
	}
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
		log.Printf("Error in GetConnection: %s\n", err.Error())
	} 
	return con, err

}

func GetDatascope(schema, id string) (types.Datascope, error) {
	var ds = types.Datascope{}	
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)

	// check if datascope exists, validating the schema along the way
	sql := "select name from "+schema+".datascopes where id=$1 and exists (select schema_name from global.customers where schema_name = $2);"
	row := tx.QueryRowContext(ctx, sql, id, schema)
	var name string
	err = row.Scan(&name)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in GetDatascope: %s\n", err.Error())
		return ds, err
	}


	ds.Id = &id
	ds.Name = name

	sql = `select  a.id, b.name, a.connection_id, a.schem, a.tabl, a.col,  a.position, a.typ, a.action, 
	a.semantics, coalesce(a.ref_schem, ''), coalesce(a.ref_tabl, ''), coalesce(a.ref_col, ''), a.dflt, a.is_nullable from ` + schema + 
	`.tables a join ` + schema + `.connections b on a.connection_id=b.id  where datascope_id=$1;`;
	rows, err := tx.QueryContext(ctx, sql, id)


	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var dr types.DatascopeRecord
			var rs, rt, rc string

			err = rows.Scan(&dr.Id, &dr.Connection, &dr.ConnectionId, &dr.Schema, &dr.Table, &dr.Col, &dr.Position, &dr.Typ, 
				&dr.Action, &dr.Semantics, &rs, &rt, &rc, &dr.Dflt, &dr.Isnullable)
			if err != nil {
				log.Printf("Error 2 in GetDatascope:  %s\n", err.Error())
				return ds, err	
			}
			var ref *types.Reference
			

			type Reference struct {
				Schema string `json:"schema"`
				Table string `json:"table"`
				Column string `json:"column"`
			 }


			if rs == "" && rt == "" && rc == "" {
				ref = nil
			} else {
				ref = &types.Reference{rs, rt, rc}
			}
			dr.Reference = ref
			ds.Records = append(ds.Records, dr)
		}

		return ds, nil
	} else {
		log.Printf("Error 3 in GetDatascope:  %s\n", err.Error())
		return ds, err		
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		return ds, err
	}	
	return ds, nil
}

func GetDatascopes(schema string) ([]types.DatascopeIdName, error) {
	sql := `select id, name from `+schema+`.datascopes where exists (select schema_name from global.customers where schema_name = $1);`

	rows, err := db.Query(sql, schema)
	if err != nil {
		log.Printf("Error 0 in GetDatascopes:  %s\n", err.Error())
		return nil, err	
	}	

	var ds = []types.DatascopeIdName{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var din types.DatascopeIdName
			err = rows.Scan(&din.Id, &din.Name)
			if err != nil {
				log.Printf("Error 1 in GetDatascopes:  %s\n", err.Error())
				return nil, err	
			}
			ds = append(ds, din)
		}
		return ds, nil
	} else {
		log.Printf("Error 2 in GetDatascopes:  %s\n", err.Error())
		return nil, err		
	}

}

func UpdateDatascope(schema string, dscope types.Datascope) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in UpdateDatascope: %s\n", err.Error())
		return err
	}

	// check if datascope exists, validating the schema along the way
	sql := "select id from "+schema+".datascopes where name=$1 and exists (select schema_name from global.customers where schema_name = $2);"
	row := tx.QueryRowContext(ctx, sql, dscope.Name, schema)
	var id string
	err = row.Scan(&id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in UpdateDatascope: %s\n", err.Error())
		return err
	}	
	log.Printf("datascope id=%s\n", id)

	// delete everything 

	sql="delete from "+schema+".tables where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 2 in UpdateDatascope: %s\n", err.Error())
		return err
	}


	if( len(dscope.Records) == 0) {
		err = tx.Commit()
		if err != nil {
			tx.Rollback()
			log.Printf("Error 6 in UpdateDatascope: %s\n", err.Error())
			return err
		}	
		return nil		
	}


	// iterate and create 
	records := dscope.Records
	for  _, r := range  records  {

		sql=`insert into ` + schema + `.tables(datascope_id, col, connection_id, schem, tabl, typ, semantics, action, position, ref_schem, ref_tabl, ref_col, dflt, is_nullable) 
		values($1, $2, (select id from ` + schema + `.connections where name=$3), $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14);`
	
		var rs, rt, rc string
		if r.Reference != nil {
			rs = r.Reference.Schema
			rt = r.Reference.Table
			rc =  r.Reference.Column
		}
		_, err = tx.ExecContext(ctx, sql, id, r.Col, r.Connection, r.Schema, r.Table, r.Typ, r.Semantics, r.Action, r.Position, rs, rt, rc, r.Dflt, r.Isnullable)
		if err != nil {
			tx.Rollback()
			log.Printf("Error 5 in UpdateDatascope record: %s\n", err.Error())
			return err
		}
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 6 in UpdateDatascope: %s\n", err.Error())
		return err
	}	
	log.Println("UpdateDatascope success!")
	return nil
}

func DeleteDatascope(schema string, id string) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in DeleteDatascope: %s\n", err.Error())
		return err
	}

	sql := "delete from "+schema+".tables where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 0 in DeleteDatascope: %s\n", err.Error())
		return err
	}

	sql = "delete from "+schema+".groupsfordatascopes where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 1 in DeleteDatascope: %s\n", err.Error())
		return err
	}	
	


	sql = "delete from "+schema+".datascopes where id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 2 in DeleteDatascope: %s\n", err.Error())
		return err
	}
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 3 in DeleteDatascope: %s\n", err.Error())
		return err
	}	
	log.Println("DeleteDatascope success!")
	return nil
}

func GetMappings(schema string) ([]types.GroupMapping, error) {

	sql := `select id, outergroup, innergroup, comment from ` + schema + `.groupmapping where exists (select schema_name from global.customers where schema_name = $1);`
	rows, err := db.Query(sql, schema)

	var mps = []types.GroupMapping{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var mp = types.GroupMapping{}
			err = rows.Scan(&mp.Id, &mp.Directorygroup, &mp.Dymiumgroup, &mp.Comments)

			if nil != err {
				log.Printf("Error in GetMappings:  %s\n", err.Error())
				return []types.GroupMapping{}, err
			} else {
				mps = append(mps, mp)
			}
		}
		
	} else {
		log.Printf("Error in GetMappings:  %s\n", err.Error())
		return mps, err
	}


	return mps, nil
}

func GetGroupAssignments(schema string) ([]types.DatascopeAndGroups, error) {
	sql := `select a.id,a.name,b.group_id,c.innergroup from `+schema+
	`.datascopes as a join `+schema+`.groupsfordatascopes as b on a.id=b.datascope_id  join `+schema+
	`.groupmapping as c on b.group_id=c.id;`

	rows, err := db.Query(sql)	
	var mps = []types.DatascopeAndGroups{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var mp = types.DatascopeAndGroups{}
			err = rows.Scan(&mp.Id, &mp.Name, &mp.Groupid, &mp.Groupname)

			if nil != err {
				log.Printf("Error in GetGroupAssignments:  %s\n", err.Error())
				return []types.DatascopeAndGroups{}, err
			} else {
				mps = append(mps, mp)
			}
		}
		
	} else {
		log.Printf("Error 1 in GetGroupAssignments:  %s\n", err.Error())
		return mps, err
	}
	return mps, nil
}
func UpdateGroupAssignment(schema string, groups types.GroupAssignment) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in UpdateGroupAssignment: %s\n", err.Error())
		return err
	}

	datascope_id := groups.Id
	sql := `delete from ` +schema+ `.groupsfordatascopes where datascope_id=$1`
	_, err = tx.ExecContext(ctx, sql, datascope_id)
	if err != nil {
		tx.Rollback()
		log.Printf("Error 2 in UpdateGroupAssignment: %s\n", err.Error())
		return err
	}	

	for  _, r := range groups.Groups {
		group_id := r.Id
		sql := `insert into ` +schema+ `.groupsfordatascopes(datascope_id, group_id) values ($1, $2);`
		_, err = tx.ExecContext(ctx, sql, datascope_id, group_id)
		if err != nil {
			tx.Rollback()
			log.Printf("Error 3 in UpdateGroupAssignment: %s\n", err.Error())
			return err
		}	
		}
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 4 in UpdateGroupAssignment: %s\n", err.Error())
		return err
	}	
	
	return nil
}

func CreateNewMapping(schema, dymiumgroup, directorygroup, comments string) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in CreateNewMapping: %s\n", err.Error())
		return err
	}

	sql := "insert into "+schema+".groupmapping(outergroup, innergroup, comment) values($1, $2, $3) ;"
	_, err = tx.ExecContext(ctx, sql, directorygroup, dymiumgroup, comments)

	if err != nil {
		tx.Rollback()
		log.Printf("Error 3 in CreateNewMapping: %s\n", err.Error())
		return err
	}	

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Printf("Error 4 in CreateNewMapping: %s\n", err.Error())
		return err
	}	
	return nil
}

func UpdateMapping(schema, id, dymiumgroup, directorygroup, comments string) error {
	sql := "update "+schema+".groupmapping set outergroup=$1, innergroup=$2, comment=$3  where id=$4;"

	_, err := db.Exec(sql, directorygroup, dymiumgroup, comments, id)
	if(err != nil) {
		log.Printf("UpdateMapping error %s\n", err.Error())
	}
	return err
}
func DeleteMapping(schema, id string) error {

	sql := "delete from "+schema+".groupmapping where id=$1;"

	_, err := db.Exec(sql, id)
	if(err != nil) {
		log.Printf("DeleteMapping error %s\n", err.Error())
	}
	return err
}
func SaveDatascope(schema string, dscope types.Datascope) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
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

	// iterate and create 
	records := dscope.Records
	for  _, r := range  records  {

		sql=`insert into ` + schema + `.tables(datascope_id, col, connection_id, schem, tabl, typ, semantics, action, position, ref_schem, ref_tabl, ref_col, dflt, is_nullable) 
		values($1, $2, (select id from ` + schema + `.connections where name=$3), $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14);`
	
		var rs, rt, rc string
		if r.Reference != nil {
			rs = r.Reference.Schema
			rt = r.Reference.Table
			rc =  r.Reference.Column
		}
		_, err = tx.ExecContext(ctx, sql, ds_id, r.Col, r.Connection, r.Schema, r.Table, r.Typ, r.Semantics, r.Action, r.Position, rs, rt, rc, r.Dflt, r.Isnullable)
		if err != nil {
			tx.Rollback()
			log.Printf("Error 4 in SaveDatascope record: %s\n", err.Error())
			return err
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
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Printf("Error in DeleteConnection: %s\n", err.Error())
		return err
	}
	sql := "select id from " + schema + ".admincredentials where connection_id=$1 and exists (select schema_name from global.customers where schema_name = $2);";

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

	return nil
}
func GetFakeAuthentication () []byte{


	token, err :=  GeneratePortalJWT("https://media-exp2.licdn.com/dms/image/C5603AQGQMJOel6FJxw/profile-displayphoto-shrink_400_400/0/1570405959680?e=1661385600&v=beta&t=MDpCTJzRSVtovAHXSSnw19D8Tr1eM2hmB0JB63yLb1s", 
	"spoofcorp", "user@spoofcorp.com", "user", []string{}, "org_nsEsSgfq3IYXe2pu")
	if(err != nil){
		log.Printf("Error: %s\n", err.Error() )
	}
	return []byte(`<html>
	<head>
	<script>
	 !function() {
		sessionStorage.setItem("Session", "`+token+`")
		window.location.href = "/app/"
	 }()
	</script>
	</head>
	<body>Callback arrived</body>
	</html>`)
	
}

func GetTunnelToken(code, auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect string) (string, string, []string, error){
	body, err := getTokenFromCode(code, auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect)

	log.Printf("returned body: %s\n", string(body))

	jsonParsed, err := gabs.ParseJSON(body)

	value, ok := jsonParsed.Path("error").Data().(string)
	if ok {
		log.Printf("Error: %s\n", value) 
		des, _ := jsonParsed.Path("error_description").Data().(string)
		
		return "", "", []string{}, errors.New(des)

	}
	access_token, ok := jsonParsed.Path("access_token").Data().(string)
	id_token, ok := jsonParsed.Path("id_token").Data().(string)
	picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_portal_domain, access_token, id_token)

	sql := fmt.Sprintf("select schema_name from global.customers where organization=$1;")
	log.Printf("sql: %s, ord_id: '%s'\n", sql, org_id)
	row := db.QueryRow(sql, org_id)
	var schema string
	err = row.Scan(&schema)


	if(err != nil){
		log.Printf("Error 1: %s\n", err.Error() )		
		return	"", "", []string{}, err
	} else{
		log.Printf("Schema: %s\n", schema )
	}

	token, err := GeneratePortalJWT(picture, schema, name, email, groups, org_id)
	if(err != nil){
		log.Printf("Error 2: %s\n", err.Error() )
	}	
	return token, name, groups, err
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
		id_token, ok := jsonParsed.Path("id_token").Data().(string)
		picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_admin_domain, access_token, id_token)

		token, err := generateAdminJWT(picture, name, email, groups, org_id)
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
		id_token, ok := jsonParsed.Path("id_token").Data().(string)
		picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_portal_domain, access_token, id_token)

		sql := fmt.Sprintf("select schema_name from global.customers where organization=$1;")
		log.Printf("sql: %s, ord_id: '%s'\n", sql, org_id)
		row := db.QueryRow(sql, org_id)
		var schema string
		err = row.Scan(&schema)


		if(err != nil){
			log.Printf("Error 1: %s\n", err.Error() )
			des, _ := jsonParsed.Path("error_description").Data().(string)
			generateError(w, r, err.Error(), des)	
			return		
		} else{
			log.Printf("Schema: %s\n", schema )
		}

		token, err := GeneratePortalJWT(picture, schema, name, email, groups, org_id)
		if(err != nil){
			log.Printf("Error 2: %s\n", err.Error() )
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
			auth_portal_domain, r.URL.RawQuery, auth_portal_client_id, url.QueryEscape(auth_portal_redirect), url.QueryEscape("openid profile email"))

		log.Printf("In /auth/login: redirect to \n%s\n", newquery)
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}
