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
	"bytes"
	"strconv"
	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	"github.com/lib/pq"
	"encoding/pem"	
	"crypto/x509"
	"os"
	"strings"
	"encoding/json"
	"encoding/hex"
	"golang.org/x/net/context"
	_"golang.org/x/exp/constraints"
	"github.com/redis/go-redis/v9"
	"time"
	"errors"
	"io"
	"crypto/rsa"
	"crypto/aes"
	"crypto/cipher"
	"github.com/Jeffail/gabs"
	"dymium.com/dymium/common"
	"dymium.com/dymium/types"
	"dymium.com/dymium/gotypes"
	"dymium.com/dymium/log"
	"path/filepath"
	"os/exec"
	"aws"
	"crypto/rand"
	"math/big"
)

var pswd = `**********`
func GenerateRandomBytes(n int) ([]byte, error) {
	b := make([]byte, n)
	_, err := rand.Read(b)

	if err != nil {
		return nil, err
	}

	return b, nil
}

func GenerateRandomString(n int) (string, error) {
	const letters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"
	ret := make([]byte, n)
	for i := 0; i < n; i++ {
		num, err := rand.Int(rand.Reader, big.NewInt(int64(len(letters))))
		if err != nil {
			return "", err
		}
		ret[i] = letters[num.Int64()]
	}
	return string(ret), nil
}

func contains[T comparable](s []T, str T) bool {
	for _, v := range s {
		if v == str {
			return true
		}
	}
	return false
}

var admins = make(map[string]int)
var users = make(map[string]int)
var rdb *redis.Client
var ctxrdb = context.Background()

func initRBAC() {
	adminnames :=  []string{"createnewconnection", "queryconnection","updateconnection","deleteconnection",
	"getconnections", "savedatascope", "updatedatascope", "deletedatascope", "getdatascopedetails",
	"createmapping", "updatemapping", "deletemapping", "getmappings", "savegroups",
	"getgroupsfordatascopes", "getselect", "getusage", "getaccesskey", "createnewconnector", 
	"getconnectors", "updateconnector", "deleteconnector", "getpolicies", "savepolicies", "querytable"}

	usernames :=  []string{"getclientcertificate", "getdatascopes", "getdatascopesaccess", "regenpassword", "getdatascopetables"}	

	for _, v := range adminnames {
		admins["/api/" + v] = 1
	}
	for _, v := range usernames {
		users["/api/" + v] = 1
	}
}
func Authorized(r *http.Request, roles []string) bool {	
	name := r.URL.Path
	for _, v := range roles {
		if(v == "admin") {
			if _, ok := admins[name]; ok {
				return true
			}
		}
		if(v == "user") {
			if _, ok := users[name]; ok {
				return true
			}
		}
	}
	log.Errorf("Error:%s  not authorized", name)
	return false
}

var psqlInfo string
var db *sql.DB
var DefaultPort = 24354
const timeOut = 20
var auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, 
	auth_admin_redirect, auth_admin_organization, auth_admin_audience string
var auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect, auth_portal_audience string
var ctx context.Context

var FilesystemRoot string
var LowMeshport, HighMeshport int

var Invoke gotypes.Invoke_t

var CaKey *rsa.PrivateKey
var CaCert *x509.Certificate 

var stdChars = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$^&*()-_=+,.?:;{}[]")

func generatePassword(passwordLength int) string {
	pword := make([]byte, passwordLength)
	length := len(stdChars)

	getChar := func() byte {
		j, err := rand.Int(rand.Reader, big.NewInt(int64(length)))
		if err != nil {
			log.Errorf("Error generating crypto strong int")
		}
		return stdChars[ int(j.Int64()) ]
	}
	for i := 0; i < passwordLength; i++ {
		pword[i] = getChar()
	}
	return string(pword)
}

func AESencrypt(plaintext []byte, keyhex string) ([]byte, error) {
	key, err := hex.DecodeString(keyhex)
    if err != nil {
        return nil, err
    }

    c, err := aes.NewCipher(key)
    if err != nil {
        return nil, err
    }

    gcm, err := cipher.NewGCM(c)
    if err != nil {
        return nil, err
    }

    nonce := make([]byte, gcm.NonceSize())
    if _, err = io.ReadFull(rand.Reader, nonce); err != nil {
        return nil, err
    }

    return gcm.Seal(nonce, nonce, plaintext, nil), nil
}

func AESdecrypt(ciphertext []byte, keyhex string) ([]byte, error) {
	key, err := hex.DecodeString(keyhex)
    if err != nil {
        return nil, err
    }

    c, err := aes.NewCipher(key)
    if err != nil {
        return nil, err
    }

    gcm, err := cipher.NewGCM(c)
    if err != nil {
        return nil, err
    }

    nonceSize := gcm.NonceSize()
    if len(ciphertext) < nonceSize {
        return nil, errors.New("ciphertext too short")
    }

    nonce, ciphertext := ciphertext[:nonceSize], ciphertext[nonceSize:]
    return gcm.Open(nil, nonce, ciphertext, nil)
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
func InitInvoke( i gotypes.Invoke_t) {
	Invoke = i
}
//aws.Invoke("DbAnalyzer", nil, bconn)
func Init(host string, port, user, password, dbname, tls string) error {
	_, err := GenerateRandomString(32)
	if(err != nil) {
		log.Errorf("Error: crypto rand failed: %s", err.Error() )
		return nil	
	}
	initRBAC()
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

	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Errorf("Opening Database, error: %s", err.Error())
		return err
	} else {
		err = db.Ping()
		if err != nil {
			log.Errorf("Opening Database, error on Ping: %s", err.Error())
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

	sports := os.Getenv("MESH_PORT_RANGE")
	ports := strings.Split(sports, "-")
	LowMeshport, _  = strconv.Atoi(ports[0])
	HighMeshport, _ = strconv.Atoi(ports[1])
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
		log.Errorf("Cert unmarshaling error: %s", err.Error() )
		return nil
	}
	
	pemcert, _ := pem.Decode([]byte(t.Certificate))

	CaCert, err = x509.ParseCertificate(pemcert.Bytes)
	if err != nil {
		log.Errorf("Cert parsing error: %s", err.Error() )
		return nil
	}

	passphrase := os.Getenv("CA_PASSPHRASE")
	CaKey, err = ParsePEMPrivateKey([]byte(t.Key), passphrase)

	if err != nil {
		log.Errorf("Key parsing error: %s", err.Error() )
		return nil
	}

	redisAddress := os.Getenv("REDIS_HOST")
	redisPort := os.Getenv("REDIS_PORT")
	redisPassword := os.Getenv("REDIS_PASSWORD")
	o := &redis.Options{
		Addr: redisAddress + ":" + redisPort,		
	}
	if redisPassword != "" {
		o.Password = redisPassword
	}
	rdb = redis.NewClient(o)	

	return nil
}

func GetDB() *sql.DB {
	return db
}

func recordLogin(schema string) error {
	err := rdb.Incr(ctx, schema+":logins").Err()
	return err
}

func recordTunnel(schema string) error {
	err := rdb.Incr(ctx, schema+":tunnels").Err()

	return err
}
func GetBytes(schema string) (int64, int64, int, int, error)  {
	bytesin, _ :=rdb.Get(ctx, schema + ":bytesin").Int64 ()
	bytesout, _ :=rdb.Get(ctx, schema + ":bytesout").Int64()
	tunnels, _ :=rdb.Get(ctx, schema + ":tunnels").Int()
	logins, err :=rdb.Get(ctx, schema + ":logins").Int()

	return  bytesin, bytesout, logins, tunnels, err
}

func GetRestrictions(schema string) (int, int, int, int, int, int, int, error)  {
	var connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels int
	sql := `select count(*) from ` +schema+`.connections`
	row := db.QueryRow(sql)
	err := row.Scan(&connections)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	
	sql = `select count(*) from ` +schema+`.datascopes`
	row = db.QueryRow(sql)
	err = row.Scan(&datascopes)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	
	sql = `select count(*) from ` +schema+`.tables where action='Block'`
	row = db.QueryRow(sql)
	err = row.Scan(&blocked)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	
	sql = `select count(*) from ` +schema+`.tables where action='Obfuscate'`
	row = db.QueryRow(sql)
	err = row.Scan(&obfuscated)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	
	sql = `select count(*) from ` +schema+`.tables where action='Redact'`
	row = db.QueryRow(sql)
	err = row.Scan(&redacted)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	

	sql = `select count(*) from ` +schema+`.tables where action='Redact'`
	row = db.QueryRow(sql)
	err = row.Scan(&redacted)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	

	sql = `select count(*) from ` +schema+`.connectorauth`
	row = db.QueryRow(sql)
	err = row.Scan(&connectors)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	

	sql = `select count(*) from ` +schema+`.connectors`
	row = db.QueryRow(sql)
	err = row.Scan(&tunnels)
	if(err != nil) {
		return connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels,err
	}	

	return  connections, datascopes, blocked, obfuscated, redacted, connectors, tunnels, nil
}
func generateAdminJWT(picture string, name string, email string, groups []string, org_id string) (string, error) {
		// generate JWT right header
		issueTime := time.Now()
		expirationTime := issueTime.Add(timeOut * time.Minute)
		
		claim := &gotypes.AdminClaims{
			// TODO
			Name: name,
			Email: email,
			Groups: groups,
			Picture: hex.EncodeToString([]byte(picture)),
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
	claim := &gotypes.AdminClaims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &gotypes.AdminClaims{
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
			log.Errorf("Error: Database Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}

	return "", errors.New("Token invalid or expired")
}
func GetSchemaFromToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

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

func ValidateAdminToken(token string) (error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.AdminClaims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	if nil == err {
		if !tkn.Valid {
			err = errors.New("No error, but invalid token")
		}
	}

	return err
}

func GetSessionFromToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

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
	return claim.Session, err

}
func GetSchemaRolesFromToken(token string) (string, []string,  []string, string, string, string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

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
	return claim.Schema, claim.Roles, claim.Groups, claim.Email, claim.Orgid, claim.Session, err

}

func UsernameFromEmail(email string) string {
	username := strings.Split(email, "@")[0]
	
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
		log.Errorf("RegenerateDatascopePassword error: %s", err.Error())
		return out, err
	}

	sql := `select distinct a.name, a.id from `+schema+`.datascopes as a  join `+schema+`.groupsfordatascopes as b on a.id=b.datascope_id join `+schema+`.groupmapping as c on c.id=b.group_id where c.outergroup = any ($1)`

    out.Username = username
	out.Password = password
	out.Schema = schema
	rows, err := db.Query(sql, pq.Array(groups))
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var ds types.DatascopeIdName
			err = rows.Scan(&ds.Name, &ds.Id) 
			if err != nil {
				log.Errorf("RegenerateDatascopePassword error: %s", err.Error())
			} else {
				out.Datascopes = append(out.Datascopes, ds)
				
			}
			rq.UserConf.Datascopes = append(rq.UserConf.Datascopes, ds.Name)
			
		}
	} else { 
		log.Errorf("RegenerateDatascopePassword error: %s", err.Error())
		return out, err
	}



	snc, _ := json.Marshal(rq)	
	_, err = Invoke("DbSync", nil, snc)
	if(err != nil) {
		log.Errorf("DatascopePassword error: syncing datascopes: %s", err.Error())
		return out, err
	}
	return out, nil
}

func GetSelect(schema string, ds *types.DatascopeTable) (types.SqlTestResult, error) {
	var conf types.SqlTestConf
	conf.Database = ds.Connection   // this is connection... name?
	conf.Schema = ds.Schema
	conf.Table = ds.Table

	var req types.Request
	req.Action = types.A_SqlTest
	req.SqlTest = &conf 
	req.Customer = schema
	req.Datascope = &ds.Datascope
	snc, _ := json.Marshal(req)	

	data, err := Invoke("DbSync", nil, snc)
	var out types.SqlTestResult
	if err != nil {
		log.Errorf("GetSelect error from Invoke: %s", err.Error()) 
		return out, err
	}

	err = json.Unmarshal(data, &out)
	if(err != nil) {
		log.Errorf("GetSelect error unmarshaling %s", err.Error())
	} 
	return out, err
}

func GetDatascopeTables(schema, id string) ([]types.DatascopeTable, error) {
	var out []types.DatascopeTable
	sql := `select distinct b.name, a.schem, a.tabl, c.name from `+schema+`.tables as a join `+schema+
	`.connections as b on a.connection_id=b.id join `+schema+`.datascopes as c on c.id=a.datascope_id where datascope_id=$1;`

	rows, err := db.Query(sql, id)
	if nil == err {
		defer rows.Close()
		var dt types.DatascopeTable
		for rows.Next() {
			err = rows.Scan(&dt.Connection, &dt.Schema, &dt.Table, &dt.Datascope) 
			if err != nil {
				return out, err
			}
			out = append(out, dt)
		}
	}	
	return out, err
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
		log.Errorf("GetDatascopesForGroups error: %s", err.Error())
		password = generatePassword(10) 
		sqlName := `insert into ` + schema + `.users (username,password)  values($1, $2);`
		_, err = db.Exec(sqlName, username, password)

		if(err != nil) {
			log.Errorf("GetDatascopesForGroups error: %s", err.Error())
		}
	}
	sql := `select distinct a.name, a.id from `+schema+`.datascopes as a  join `+schema+`.groupsfordatascopes as b on a.id=b.datascope_id join `+schema+`.groupmapping as c on c.id=b.group_id where c.outergroup = any ($1)`


	out.Schema = schema

	rq.UserConf.Name = username
	rq.UserConf.Password = password

	rows, err := db.Query(sql, pq.Array(groups))
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var ds types.DatascopeIdName
			err = rows.Scan(&ds.Name, &ds.Id) 
			if err != nil {
				log.Errorf("GetDatascopesForGroups error: %s", err.Error())
			} else {
				out.Datascopes = append(out.Datascopes, ds)
				
			}
			rq.UserConf.Datascopes = append(rq.UserConf.Datascopes, ds.Name)			
		}
	} else { 
		log.Errorf("GetDatascopesForGroups error: %s", err.Error())
	}

	snc, _ := json.Marshal(rq)	
	_, err = Invoke("DbSync", nil, snc)

	if age >  60*60*24 {
		password = pswd
	}
    out.Username = username
	out.Password = password

	if(err != nil) {
		log.Errorf("GetDatascopesForGroups error: %s", err.Error())
	}
	return out, err
}

func GetIdentityFromToken(token string) (string, []string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	if nil == err {
		if !tkn.Valid {
			err = errors.New("GetIdentityFromToken: no error, but invalid token")
		}
	}
	if claim.Schema == "" {
		err = errors.New("Empty schema")
	}
	return claim.Email, claim.Groups, err
}

func SavePolicies(schema string, body []byte) error {
	sqlst := `INSERT INTO ` + schema + `.policies (policy)
	VALUES($1) 
	ON CONFLICT ON CONSTRAINT constr_id_unique
	DO 
	   UPDATE SET policy=$1`
	_, err := db.Exec(sqlst, body)

	return err
}

func GetPolicies(schema string) ([]byte, error) {
	sqlst := `select policy from ` + schema + `.policies;`
	row := db.QueryRow(sqlst)
	var policy []byte

	err := row.Scan(&policy)
	if err == sql.ErrNoRows {
		err = nil
		policy = []byte(`{"error":"no record"}`)
	}
	return policy, err
}

func CreateNewConnection(schema string, con types.ConnectionRecord) (string, error) {
	var row *sql.Row
	var id, sqlI string

	if con.Usesconnector {
		sqlI = `insert into `+schema+`.connections(name, database_type, dbname, use_tls, description,
			use_connector,   
			connector_id,  tunnel_id, address, port) 
			values($1,$2,$3,$4,$5,$6,$7,$8,$9,$10) returning id; `

		row = db.QueryRow(sqlI, con.Name, con.Dbtype, con.Dbname, con.UseTLS, 
			con.Description, con.Usesconnector,
			con.Connectorid, con.Tunnelid, "", 0)	 
	} else {
		sqlI = `insert into `+schema+`.connections(name, database_type, address, 
			port, dbname, use_tls, description,
			use_connector) 
			values($1,$2,$3,$4,$5,$6,$7,$8) returning id; `

		row = db.QueryRow(sqlI, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, 
			con.Description, con.Usesconnector)
	}
	err := row.Scan(&id)
	if err != nil {
		return "", err
	}
	// now let's save credentials!
	var credid string
	sqlI = `insert into `+schema+`.admincredentials(connection_id, username) values($1, $2) returning id`
	row = db.QueryRow(sqlI, id, con.Username)
	err = row.Scan(&credid)
	if err != nil {
		return id, err
	}
	hexkey := os.Getenv( strings.ToUpper(schema) + "_KEY" )

	enc, err := AESencrypt([]byte(*con.Password), hexkey)
	if err != nil {
		return id, err
	}

	sqlI = `insert into `+schema+`.passwords(id, password) values($1, $2)`;
	
	_, err = db.Exec(sqlI, credid, enc)
	if err != nil {
		return id, err
	}

	return id, nil
}
func GetConnections(schema string) ([]types.ConnectionRecord, error ) {
	sql := `select a.id,a.name,COALESCE(a.address,''),COALESCE(a.port, 0),a.dbname,a.database_type,a.use_tls,a.description,b.username,
	b.id,a.use_connector,COALESCE(a.connector_id,''),COALESCE(a.tunnel_id,''),COALESCE(c.name, ''),COALESCE(d.connectorname, '') from `+
		schema+`.connections as a join `+
		schema+`.admincredentials as b on a.id=b.connection_id left join ` + 
		schema + `.connectorauth as c on c.id=a.connector_id left join ` + 
		schema +`.connectors as d on d.id=a.tunnel_id	;`
	
	rows, err := db.Query(sql)

	var conns = []types.ConnectionRecord{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var conn = types.ConnectionRecord{}
			err = rows.Scan(&conn.Id, &conn.Name, &conn.Address, &conn.Port, &conn.Dbname, &conn.Dbtype, &conn.UseTLS, 
					&conn.Description, &conn.Username, &conn.Credid, &conn.Usesconnector, &conn.Connectorid, &conn.Tunnelid,
					&conn.Connectorname, &conn.Tunnelname)
			if nil != err {
				log.Errorf("GetConnections error:  %s", err.Error())
				return []types.ConnectionRecord{}, err
			} else {
				conns = append(conns, conn)
			}
		}
	} else {
		log.Errorf("GetConnections error:  %s", err.Error())
		return conns, err
	}

	return conns, nil
}

func  CheckAndRefreshToken(token string, sport string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()
	nport, _ := strconv.Atoi(sport)

	if err == nil && tkn.Valid {
		// update token

		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &gotypes.Claims{
			Name: claim.Name,
			Email: claim.Email,
			Groups: claim.Groups,
			Roles: claim.Roles,
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
			log.Errorf("CheckAndRefreshToken error: Token Signing Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}	

return "", err
}
func GetRoles(schema string, groups []string, admin_group string) []string {
	var roles []string
	log.Infof("GetRoles: groups: %v\n", groups)
	if(len(groups) == 0) {
		return roles;
	}
	sql := `select outergroup, adminaccess from ` + schema + `.groupmapping where outergroup = any ($1);`;

	var hasadmin, hasuser bool

	log.Infof("GetRoles: sql: %s\n", sql)
	rows, err := db.Query(sql, pq.Array(groups))
	if(err != nil) {
		log.Errorf("GetRoles error quering mapping: %s", err.Error())
		return roles;
	} else {
		defer rows.Close()


		var group string
		var admin bool
		for rows.Next() {
			err = rows.Scan(&group, &admin)
			if(err != nil) {
				log.Errorf("GetRoles error quering mapping: %s", err.Error())
				return roles;
			}
			if admin {
				hasadmin = true
			}
			hasuser = true
		}
		if hasadmin {
			roles = append(roles, gotypes.RoleAdmin)	
		}
		if hasuser {
			roles = append(roles, gotypes.RoleUser)	
		}
	}
	if !hasadmin && !hasuser {
		// check if there are any group mappings
		log.Infof("In GetRoles, not admin, not user")
		sql := `select count(*) from ` + schema + `.groupmapping;`;
		row := db.QueryRow(sql)
		var count int
		err := row.Scan(&count)
		log.Infof("In GetRoles, count: %d, err: %v", count, err)

		if err == nil && count == 0 {
			// do the privilege elevation
			for _, gr := range groups {
				if gr == admin_group {
					roles = append(roles, gotypes.RoleAdmin)
					roles = append(roles, gotypes.RoleUser)	
					roles = append(roles, gotypes.RoleInstaller)	
					log.Infof("%s == %s", gr, admin_group)
					
					break
				} else {
					log.Infof("%s != %s", gr, admin_group)
				}
			}
		}
	}
	return roles
}
func GeneratePortalJWT(picture string, schema string, name string, email string, groups []string, roles []string, org_id  string) (string, error) {
	// generate JWT right header
	issueTime := time.Now()
	expirationTime := issueTime.Add(timeOut * time.Minute)
	p := hex.EncodeToString([]byte(picture))

	session, _ := GenerateRandomString(32) //ignore the error for now
	claim := &gotypes.Claims{
		// TODO
		Name: name,
		Session: session,
		Groups: groups,
		Roles: roles,
		Email: email,
		Picture:  p,
		Schema: schema,
		Port: DefaultPort,
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

	return tokenString, err
}

func refreshPortalToken(token string) (string, error) {
	jwtKey := []byte(os.Getenv("SESSION_SECRET"))
	claim := &gotypes.Claims{}

	tkn, err := jwt.ParseWithClaims(token, claim, func(token *jwt.Token) (interface{}, error) {
		return jwtKey, nil
	})
	timeNow := time.Now()

	if err == nil && tkn.Valid {
		// update token
		expirationTime := timeNow.Add(timeOut * time.Minute)
		newclaim := &gotypes.Claims{
			Name: claim.Name,
			Session: claim.Session,
			Email: claim.Email,
			Groups: claim.Groups,
			Roles: claim.Roles,
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
			log.Errorf("refreshPortalToken error: Token Signing Problem: %s", err)

			// If there is an error in creating the JWT return an internal server error
			return "", errors.New("JWT Creation Problem")
		}

		return tokenString, nil
	}

	return "", err
}

func generateError(w http.ResponseWriter, r *http.Request, header string, body string) error {
	/*
	Failed to get userinfo: "+err.Error()
	*/
	log.Errorf("Error %s: %s", header, body)
	nonce, _ := GenerateRandomString(32)
	common.CommonNocacheNocspHeaders(w, r)
	w.Header().Set("Content-Security-Policy", "script-src 'nonce-"+nonce+"'")	
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(`<html>
		<head>
		<script nonce="`+nonce+`">
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
		log.Errorf("getUserInfoFromToken error: %s", err.Error()) 
		return "", "", "", []string{}, "",  err

	}
	nr.Header.Add("Authorization", "Bearer " + token)
	
	resp, err := client.Do(nr)

	if err != nil {
		log.Errorf("getUserInfoFromToken error: %s", err.Error()) 
		return "", "", "", []string{}, "", err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)	

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
		log.Errorf("getTokenFromCode error: %s", err.Error()) 
		return []byte{}, err

	}
	nr.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	resp, _ := client.Do(nr)
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)

	return body, err
}

func GetSchemaFromClientId(clientid string) (string, error) {
	sql := `select schema_name from global.customers where organization=$1;`

	row := db.QueryRow(sql, clientid)
	var schema string
	err := row.Scan(&schema)
	return schema, err
}

func GetClientIdFromSchema(schema string) (string, error) {
	sql := `select organization from global.customers where schema_name=$1;`

	row := db.QueryRow(sql, schema)
	var clientid  string
	err := row.Scan(&clientid)
	return clientid, err
}

func DecryptPassword(schema string, password []byte) (string, error) {
	hexkey := os.Getenv( strings.ToUpper(schema) + "_KEY" )
	plain, err := AESdecrypt(password, hexkey)

	if err != nil {
		return "", err
	}
	return string(plain), nil
} 

func UpdateConnection(schema string, con types.ConnectionRecord) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Errorf("UpdateConnection error in UpdateConnection: %s", err.Error())
		return err
	}
	var sq string
	var row *sql.Row
	if con.Usesconnector {
		sq = `update `+schema+`.connections set name=$1, database_type=$2, 
			address=$3, port=$4, dbname=$5, use_tls=$6, description=$7,use_connector=$8,connector_id=$9,tunnel_id=$10
			 where id=$11  
			and exists (select schema_name from global.customers where schema_name = $12) returning id; `
			row  = tx.QueryRowContext(ctx, sq, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, 
				con.UseTLS, con.Description, con.Usesconnector, 
				con.Connectorid, con.Tunnelid, con.Id, schema)

	} else {
		sq = `update `+schema+`.connections set name=$1, database_type=$2, 
			address=$3, port=$4, dbname=$5, use_tls=$6, description=$7,use_connector=$8 where id=$9  
			and exists (select schema_name from global.customers where schema_name = $10) returning id; `
			row  = tx.QueryRowContext(ctx, sq, con.Name, con.Dbtype, con.Address, con.Port, con.Dbname, con.UseTLS, con.Description, con.Usesconnector, con.Id, schema)
	}
	// fmt.Printf("\nsql: %s\ncon: %s \n", sq, con.Dbtype)
	var newid string
	err = row.Scan(&newid)
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateConnection Error 1: %s", err.Error())
		return err
	}
	if(con.Username != nil && con.Password != nil) {
		sq = `update `+schema+`.admincredentials set username=$1 where connection_id=$2 returning id;`
		row  := tx.QueryRowContext(ctx, sq, con.Username, con.Id)
		var credid string
		err = row.Scan(&credid)
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateConnection 2: %s", err.Error())
			return err
		}
		hexkey := os.Getenv( strings.ToUpper(schema) + "_KEY" )

		enc, err := AESencrypt([]byte(*con.Password), hexkey)
		if err != nil {
			return  err
		}

		sq = `update `+schema+`.passwords set password=$1 where id=$2;`
		_, err  = tx.ExecContext(ctx, sq, enc, credid)
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateConnection Error 3: %s", err.Error())
			return err
		}		
	}
	/*
	if(con.Username != nil && con.Password != nil) {
		log.Printf("Username: %s, password %s", *con.Username, *con.Password )
	}
	*/
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		return err
	}

	return nil
}
func GetConnectorAddress(schema, tunnel_id string) ( string, int, error) {
	var localport int
	sql := `select localport from ` + schema + `.connectors where id=$1;`

	row := db.QueryRow(sql, tunnel_id)
	err := row.Scan(&localport) 

	host := os.Getenv("CONNECTOR_DOMAIN")
	return schema + host, localport, err
}
func GetConnection(schema, id string) (types.ConnectionParams, error) {
	sql := `select a.database_type,a.address,a.port,b.username,c.password,a.dbname, a.use_tls, a.use_connector,
	coalesce(a.connector_id, ''), coalesce(a.tunnel_id, '') from 
		`+schema+`.connections as a join `+schema+`.admincredentials as b on a.id=b.connection_id 
			join `+schema+`.passwords as c on b.id=c.id where a.id=$1;`

	row := db.QueryRow(sql, id)
	var con types.ConnectionParams
	var use_connector bool
	var connector_id, tunnel_id string

	var password []byte
	err := row.Scan(&con.Typ, &con.Address, &con.Port, &con.User, &password, 
		&con.Database, &con.Tls, &use_connector, &connector_id, &tunnel_id)

	if(err != nil) {
		log.Errorf("GetConnection error 0: %s", err.Error())
		return con, err
	} else {
		hexkey := os.Getenv( strings.ToUpper(schema) + "_KEY" )
		plain, err := AESdecrypt(password, hexkey)
	
		if err != nil {
			fmt.Printf( "GetConnection error X: %s\n", err.Error() )
		}
		con.Password = string(plain)

		if use_connector {
			// get the connector information
			var localport int
			sql = `select localport from ` + schema + `.connectors where id=$1;`

			row := db.QueryRow(sql, tunnel_id)
			err := row.Scan(&localport) 
			if(err != nil) {
				log.Errorf("GetConnection error 1: %s", err.Error())
				return con, err
			}
			host := os.Getenv("CONNECTOR_DOMAIN")
			con.Address = schema + host
			con.Port = localport
		}
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
		log.Errorf("GetDatascope Error 1: %s", err.Error())
		return ds, err
	}


	ds.Id = &id
	ds.Name = name

	sql = `select  a.id, b.name, a.connection_id, a.schem, a.tabl, a.col,  a.position, a.typ, a.action, 
	a.semantics, coalesce(a.ref_schem, ''), coalesce(a.ref_tabl, ''), coalesce(a.ref_col, ''), a.dflt, a.is_nullable, a.possible_actions from ` + schema + 
	`.tables a join ` + schema + `.connections b on a.connection_id=b.id  where datascope_id=$1;`;
	rows, err := tx.QueryContext(ctx, sql, id)


	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var dr types.DatascopeRecord
			var rs, rt, rc string

			err = rows.Scan(&dr.Id, &dr.Connection, &dr.ConnectionId, &dr.Schema, &dr.Table, &dr.Col, &dr.Position, &dr.Typ, 
				&dr.Action, &dr.Semantics, &rs, &rt, &rc, &dr.Dflt, &dr.Isnullable, pq.Array(&dr.PossibleActions))
			if err != nil {
				log.Errorf("GetDatascope Error 2:  %s", err.Error())
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
		log.Errorf("GetDatascope Error 3:  %s", err.Error())
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
		log.Errorf("GetDatascopes Error 0:  %s", err.Error())
		return nil, err	
	}	

	var ds = []types.DatascopeIdName{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var din types.DatascopeIdName
			err = rows.Scan(&din.Id, &din.Name)
			if err != nil {
				log.Errorf("GetDatascopes Error 1:  %s", err.Error())
				return nil, err	
			}
			ds = append(ds, din)
		}
		return ds, nil
	} else {
		log.Errorf("GetDatascopes Error 2:  %s", err.Error())
		return nil, err		
	}

}

func UpdateDatascope(schema string, dscope types.Datascope) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Errorf("UpdateDatascope error: %s", err.Error())
		return err
	}

	// check if datascope exists, validating the schema along the way
	sql := "select id from "+schema+".datascopes where name=$1 and exists (select schema_name from global.customers where schema_name = $2);"
	row := tx.QueryRowContext(ctx, sql, dscope.Name, schema)
	var id string
	err = row.Scan(&id)
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateDatascope error 1: %s", err.Error())
		return err
	}	
	// delete everything 

	sql="delete from "+schema+".tables where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateDatascope error 2: %s", err.Error())
		return err
	}


	if( len(dscope.Records) == 0) {
		err = tx.Commit()
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateDatascope error 3: %s", err.Error())
			return err
		}	
		return nil		
	}


	// iterate and create 
	records := dscope.Records
	for  _, r := range  records  {

		sql=`insert into ` + schema + `.tables(datascope_id, col, connection_id, schem, tabl, typ, semantics, action, position, ref_schem, ref_tabl, ref_col, dflt, is_nullable, possible_actions) 
		values($1, $2, (select id from ` + schema + `.connections where name=$3), $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15);`
	
		var rs, rt, rc string
		if r.Reference != nil {
			rs = r.Reference.Schema
			rt = r.Reference.Table
			rc =  r.Reference.Column
		}
		_, err = tx.ExecContext(ctx, sql, id, r.Col, r.Connection, r.Schema, r.Table, r.Typ, r.Semantics, r.Action, r.Position, rs, rt, rc, r.Dflt, r.Isnullable, pq.Array(r.PossibleActions))
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateDatascope Error 3: %s", err.Error())
			return err
		}
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateDatascope Error 4: %s", err.Error())
		return err
	}	

	return nil
}

func DeleteDatascope(schema string, id string) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Errorf("DeleteDatascope error: %s", err.Error())
		return err
	}

	sql := "delete from "+schema+".tables where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteDatascope error 0: %s", err.Error())
		return err
	}

	sql = "delete from "+schema+".groupsfordatascopes where datascope_id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteDatascope Error 1: %s", err.Error())
		return err
	}	
	


	sql = "delete from "+schema+".datascopes where id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteDatascope error 2: %s", err.Error())
		return err
	}
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteDatascope error 3: %s", err.Error())
		return err
	}	

	return nil
}

func GetMappings(schema string) ([]types.GroupMapping, error) {

	sql := `select id, outergroup, innergroup, comment, adminaccess from ` + schema + `.groupmapping where exists (select schema_name from global.customers where schema_name = $1);`
	rows, err := db.Query(sql, schema)

	var mps = []types.GroupMapping{}
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var mp = types.GroupMapping{}
			err = rows.Scan(&mp.Id, &mp.Directorygroup, &mp.Dymiumgroup, &mp.Comments, &mp.Adminaccess)

			if nil != err {
				log.Errorf("GetMappings error:  %s", err.Error())
				return []types.GroupMapping{}, err
			} else {
				mps = append(mps, mp)
			}
		}
		
	} else {
		log.Errorf("GetMappings error:  %s", err.Error())
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
				log.Errorf("GetGroupAssignments error:  %s", err.Error())
				return []types.DatascopeAndGroups{}, err
			} else {
				mps = append(mps, mp)
			}
		}
		
	} else {
		log.Errorf("GetGroupAssignments error:  %s", err.Error())
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
		log.Errorf("UpdateGroupAssignment error: %s", err.Error())
		return err
	}

	datascope_id := groups.Id
	sql := `delete from ` +schema+ `.groupsfordatascopes where datascope_id=$1`
	_, err = tx.ExecContext(ctx, sql, datascope_id)
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateGroupAssignment error 2: %s", err.Error())
		return err
	}	

	for  _, r := range groups.Groups {
		group_id := r.Id
		sql := `insert into ` +schema+ `.groupsfordatascopes(datascope_id, group_id) values ($1, $2);`
		_, err = tx.ExecContext(ctx, sql, datascope_id, group_id)
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateGroupAssignment error 3: %s", err.Error())
			return err
		}	
		}
	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateGroupAssignment error 4: %s", err.Error())
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
		log.Errorf("CreateNewMapping error: %s", err.Error())
		return err
	}

	sql := "insert into "+schema+".groupmapping(outergroup, innergroup, comment) values($1, $2, $3) ;"
	_, err = tx.ExecContext(ctx, sql, directorygroup, dymiumgroup, comments)

	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewMapping error 3: %s", err.Error())
		return err
	}	

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewMapping error 4: %s", err.Error())
		return err
	}	
	return nil
}

func UpdateMapping(schema, id, dymiumgroup, directorygroup, comments string, adminaccess bool) error {
	sql := "update "+schema+".groupmapping set outergroup=$1, innergroup=$2, comment=$3, adminaccess=$4  where id=$5;"

	_, err := db.Exec(sql, directorygroup, dymiumgroup, comments, adminaccess, id)
	if(err != nil) {
		log.Errorf("UpdateMapping error %s", err.Error())
	}
	return err
}
func DeleteMapping(schema, id string) error {

	sql := "delete from "+schema+".groupmapping where id=$1;"

	_, err := db.Exec(sql, id)
	if(err != nil) {
		log.Errorf("DeleteMapping error %s", err.Error())
	}
	return err
}
func SaveDatascope(schema string, dscope types.Datascope) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Errorf("SaveDatascope error: %s", err.Error())
		return err
	}

	// check if datascope exists, validating the schema along the way
	sql := "select count(id) from "+schema+".datascopes where name=$1 and exists (select schema_name from global.customers where schema_name = $2);"
	row := tx.QueryRowContext(ctx, sql, dscope.Name, schema)
	var count int
	err = row.Scan(&count)
	if err != nil {
		tx.Rollback()
		log.Errorf("SaveDatascope error 1: %s", err.Error())
		return err
	}	
	if(count != 0) {
		tx.Rollback()
		err := errors.New("A record for datascope '" + dscope.Name + "' already exists!")
		log.Errorf("SaveDatascope error 2: %s", err.Error())
		return err
	}
	sql="insert into "+schema+".datascopes(name) values($1)  returning id;"
	row = tx.QueryRowContext(ctx, sql, dscope.Name)
	var ds_id string
	err = row.Scan(&ds_id)	
	if err != nil {
		tx.Rollback()
		log.Errorf("SaveDatascope error 3: %s", err.Error())
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
			log.Errorf("SaveDatascope error 4: %s", err.Error())
			return err
		}
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("SaveDatascope error 5: %s", err.Error())
		return err
	}	

	return nil
}
func DeleteConnection(schema, id string) error {
	// Create a new context, and begin a transaction
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		log.Errorf("DeleteConnection error: %s", err.Error())
		return err
	}
	sql := "select id from " + schema + ".admincredentials where connection_id=$1 and exists (select schema_name from global.customers where schema_name = $2);";

	row := tx.QueryRowContext(ctx, sql, id, schema)
	var credid string
	err = row.Scan(&credid)
	if err != nil {
		log.Errorf("DeleteConnection error 2: %s", err.Error())
		return err
	}
	sql = "delete from "+schema+".passwords where id=$1;"
	_, err = tx.ExecContext(ctx, sql, credid)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnection error 3: %s", err.Error())
		return err
	}
	sql = "delete from "+schema+".admincredentials where id=$1;"
	_, err = tx.ExecContext(ctx, sql, credid)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnection error 4: %s", err.Error())
		return err
	}
	sql = "delete from "+schema+".connections where id=$1;"
	_, err = tx.ExecContext(ctx, sql, id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnection error 5: %s", err.Error())
		return err
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnection error 6: %s", err.Error())
		return err
	}

	return nil
}
func GetFakeAuthentication () []byte{


	token, err :=  GeneratePortalJWT("https://media-exp2.licdn.com/dms/image/C5603AQGQMJOel6FJxw/profile-displayphoto-shrink_400_400/0/1570405959680?e=1661385600&v=beta&t=MDpCTJzRSVtovAHXSSnw19D8Tr1eM2hmB0JB63yLb1s", 
	"spoofcorp", "user@spoofcorp.com", "user", []string{"Admins", "Users"}, []string{"user", "admin"}, "org_nsEsSgfq3IYXe2pu")
	if(err != nil){
		log.Errorf("Error: %s", err.Error() )
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

func CheckConnectorAuth(schema, key, secret string) error {
	sql := `select accesssecret from ` + schema + `.connectorauth where accesskey=$1;`

	row  := db.QueryRow(sql, key)
	var realsecret string
	err := row.Scan(&realsecret)
	if err != nil {
		return err
	}
	if  secret != realsecret {
		return errors.New("Invalid secret")
	}
	return nil
}

func GetTargets(schema, key, secret string, ) ([]string, error) {
	var targets []string 	
	sql := `select a.targetaddress, a.targetport, a.localport, b.id, a.id from ` + schema + `.connectors as a 
	join `+schema+`.connectorauth as b on a.id_connectorauth=b.id where b.accesskey=$1 and b.accesssecret=$2;`

	rows, err  := db.Query(sql, key, secret)
	if nil == err {
		defer rows.Close()
		for rows.Next() {		
			var address string
			var port, localport int
			var connid, tunnelid string
			err := rows.Scan(&address, &port, &localport, &connid, &tunnelid)	
			if err != nil {
				log.Errorf("Error: %s\n", err.Error())
				return nil, err
			}
			s := fmt.Sprintf("%s:%d,%d,%s,%s", address, port, localport, connid, tunnelid)
			log.Infof("Add target %s", s)
			targets = append(targets, s)
		}
	} else {
		return nil, err
	}


	return targets, nil
}

func GetTunnelToken(code, auth_portal_domain, auth_portal_client_id, 
	auth_portal_client_secret, auth_portal_redirect string) (string, string, []string, string, string, []string, error){
	body, err := getTokenFromCode(code, auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect)

	jsonParsed, err := gabs.ParseJSON(body)

	value, ok := jsonParsed.Path("error").Data().(string)
	if ok {
		log.Errorf("GetTunnelToken error: %s", value) 
		des, _ := jsonParsed.Path("error_description").Data().(string)
		
		return "", "", []string{}, "", "", []string{}, errors.New(des)

	}
	access_token, ok := jsonParsed.Path("access_token").Data().(string)
	id_token, ok := jsonParsed.Path("id_token").Data().(string)
	picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_portal_domain, access_token, id_token)
	
	sql := fmt.Sprintf("select schema_name from global.customers where organization=$1;")

	row := db.QueryRow(sql, org_id)
	var schema string
	err = row.Scan(&schema)


	if(err != nil){
		log.ErrorUserf(schema, "", email, groups, GetRoles(schema, groups, ""), "Error in tunnel login: %s", err.Error())
	
		return	"", "", []string{}, "", "", []string{}, err
	} 

	token, err := GeneratePortalJWT(picture, schema, name, email, groups, GetRoles(schema, groups, ""), org_id)

	session, _ := GetSessionFromToken(token)

	if(err != nil){
		log.ErrorUserf(schema, session, email, groups, []string{}, "Error in tunnel JWT generation: %s", err.Error())
	}	
	recordTunnel(schema)
	if(err == nil) {
		log.InfoUserf(schema, session, email, groups, []string{}, "Successful tunnel login")
	}
	return token, name, groups, schema, email, GetRoles(schema, groups, ""), err
}
func AuthenticationAdminHandlers(h *mux.Router) error {
	host := os.Getenv("ADMIN_HOST")
	log.Debugf("ADMIN_HOST: %s", host)	
	a := h.Host(host).Subrouter()

	a.HandleFunc("/auth/refresh", func(w http.ResponseWriter, r *http.Request) {
		var status types.AuthStatus

		token := common.TokenFromHTTPRequest(r)

		newtoken, autherror := refreshAdminToken(token)
		if autherror == nil {
			status = types.AuthStatus{"OK", "Session live", newtoken}
		} else {
			log.Error("Return error, reauthenticate")
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

	a.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		error, _ := mux.Vars(r)["error"]
		error_description, _ := mux.Vars(r)["error_description"]		

		generateError(w, r, error, error_description)		
		return		
	}).
	Queries("error", "{error}").
    Queries("error_description", "{error_description}").Methods("GET")

	a.HandleFunc("/auth/redirect", func(w http.ResponseWriter, r *http.Request) {
		code := r.URL.Query().Get("code")
		if code == "" {
			generateError(w, r, "Error: ", r.URL.Query().Get("error_description"))
			return
		} 

		body, err := getTokenFromCode(code, auth_admin_domain, auth_admin_client_id, auth_admin_client_secret, auth_admin_redirect)

		jsonParsed, err := gabs.ParseJSON(body)

		value, ok := jsonParsed.Path("error").Data().(string)
		if ok {
			log.Errorf("Redirect error: %s", value) 
			des, _ := jsonParsed.Path("error_description").Data().(string)
			generateError(w, r, value, des)
			return

		}
		access_token, ok := jsonParsed.Path("access_token").Data().(string)
		id_token, ok := jsonParsed.Path("id_token").Data().(string)
		picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_admin_domain, access_token, id_token)
		fmt.Printf("id_token: %s\n", id_token)
		fmt.Printf("access_token: %s\n", access_token)
		fmt.Printf("name: %s, email: %s\n",  name, email)
		token, err := generateAdminJWT(picture, name, email, groups, org_id)
		if(err != nil){
			log.Errorf("Redirect error: %s", err.Error() )
		}
		nonce, _ := GenerateRandomString(32)
		common.CommonNocacheNocspHeaders(w, r)
		w.Header().Set("Content-Type", "text/html")
		w.Header().Set("Content-Security-Policy", "script-src 'self' 'nonce-"+nonce+"'")

		w.Write([]byte(`<html>
		<head>
		<script nonce="`+nonce+`">
		 !function() {
			sessionStorage.setItem("Session", "`+token+`")
			window.location.href = "/app/"
		 }()
		</script>
		</head>
		<body>Callback arrived</body>
		</html>`))
		
	}).Methods("GET")

	a.HandleFunc("/auth/login", func(w http.ResponseWriter, r *http.Request) {
		org := r.URL.Query().Get("organization")
		var newquery string
		if org == "" {
			newquery = fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&organization=%s&scope=%s&prompt=login", 
				auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, url.QueryEscape(auth_admin_redirect),
				auth_admin_organization, url.QueryEscape("groups") )
		} else {
			newquery = fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&scope=%s&prompt=login", 
				auth_admin_domain, r.URL.RawQuery, auth_admin_client_id, url.QueryEscape(auth_admin_redirect), url.QueryEscape("groups") )

		}
	
		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}

func AuthenticationPortalHandlers(h *mux.Router) error {
	host := os.Getenv("CUSTOMER_HOST")
	log.Debugf("CUSTOMER_HOST: %s", host)
	p := h.Host(host).Subrouter()

	p.HandleFunc("/auth/refresh", func(w http.ResponseWriter, r *http.Request) {
		var status types.AuthStatus

		token := common.TokenFromHTTPRequest(r)

		newtoken, autherror := refreshPortalToken(token)
		if autherror == nil {
			status = types.AuthStatus{"OK", "Session live", newtoken}
		} else {
			log.Debug("Return error, reauthenticate")
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
		} 

		body, err := getTokenFromCode(code, auth_portal_domain, auth_portal_client_id, auth_portal_client_secret, auth_portal_redirect)

		jsonParsed, err := gabs.ParseJSON(body)

		value, ok := jsonParsed.Path("error").Data().(string)
		if ok {
			log.Errorf("Redirect error: %s", value) 
			des, _ := jsonParsed.Path("error_description").Data().(string)
			generateError(w, r, value, des)
			return

		}
		access_token, ok := jsonParsed.Path("access_token").Data().(string)
		id_token, ok := jsonParsed.Path("id_token").Data().(string)
		picture, name, email, groups, org_id, err := getUserInfoFromToken(auth_portal_domain, access_token, id_token)

		log.Infof("ID Tolen: %s", id_token)
		log.Infof("Access Tolen: %s", access_token)
		log.Infof("Groupsn: %v", groups)

		sql := fmt.Sprintf("select schema_name, admin_group from global.customers where organization=$1;")

		row := db.QueryRow(sql, org_id)
		var schema string
		var admin_group string
		err = row.Scan(&schema, &admin_group)


		if(err != nil){
			log.Errorf("Redirect error 1: %s", err.Error() )
			des, _ := jsonParsed.Path("error_description").Data().(string)
			log.ErrorTenantf(org_id,  "Error in retrieving tenant: %s", err.Error() )

			generateError(w, r, err.Error(), des)	
			return		
		} 

		token, err := GeneratePortalJWT(picture, schema, name, email, groups, GetRoles(schema, groups, admin_group), org_id)
		session, _ := GetSessionFromToken(token)
		if(err != nil){
			log.ErrorUserf(schema, session, email, groups,  GetRoles(schema, groups, admin_group), "Error in portal JWT generation: %s", err.Error() )
		}
		recordLogin(schema)
		if(err == nil){
			log.InfoUserf(schema, session, email, groups,  GetRoles(schema, groups, admin_group), "Successful portal login, token: %s", token)
		}
		nonce, _ := GenerateRandomString(32)
		common.CommonNocacheNocspHeaders(w, r)
		w.Header().Set("Content-Type", "text/html")
		w.Header().Set("Content-Security-Policy", "default-src 'self'; frame-ancestors 'self'; form-action 'self'; script-src 'self' 'nonce-"+nonce+"'")

		w.Write([]byte(`<html>
		<head>
		<script nonce="`+nonce+`">
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
		newquery := fmt.Sprintf("%sauthorize?%s&response_type=code&client_id=%s&redirect_uri=%s&scope=%s&prompt=login", 
			auth_portal_domain, r.URL.RawQuery, auth_portal_client_id, url.QueryEscape(auth_portal_redirect), url.QueryEscape("openid profile email"))

		http.Redirect(w, r, newquery, http.StatusFound)
	})


	return nil
}

func getports(schema string, ctx context.Context, tx *sql.Tx) ([]int, error) {
	var ports  []int
	sqll := `select localport from ` + schema + `.connectors`
	rows, err  := tx.QueryContext(ctx, sqll)
	if nil == err {
		defer rows.Close()
		for rows.Next() {
			var p int 
			rows.Scan(&p)
			ports = append(ports, p)
		}
	} else {
		log.Errorf("CreateNewConnector error x: %s", err.Error())
		return []int{}, err
	}
	return ports, nil
}
func getport(ports []int) (int, error) {
	for i := LowMeshport; i < HighMeshport; i++ {
		if !contains(ports, i) {
			return i, nil
		}
	}
	return 0, errors.New("No ports available")
}
func CreateNewConnector(schema string, req *types.AddConnectorRequest) (string, error) {
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewConnector error 0: %s", err.Error())
		return "", err
	}	

	sql := `insert into `+schema+`.connectorauth(name, accesskey, accesssecret) values($1,$2,$3) returning id;`

	var id string
	row  := tx.QueryRowContext(ctx, sql, req.Name, req.Accesskey, req.Secret)
	err = row.Scan(&id)
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewConnector error 1: %s", err.Error())
		return "", err
	}	


	for i := 0; i < len(req.Tunnels); i++ {
		ports, err := getports(schema, ctx, tx)

		if err != nil {
			tx.Rollback()
			log.Errorf("CreateNewConnector error 2: %s", err.Error())
			return "", err
		}	

		localport, err := getport(ports)

		if err != nil {
			tx.Rollback()
			log.Errorf("CreateNewConnector error 3: %s", err.Error())
			return "", err
		}	
		sql = `insert into `+schema+`.connectors(targetaddress,targetport,connectorname,id_connectorauth, localport) values($1,$2,$3,$4,$5) `
		_, err = tx.ExecContext(ctx, sql, req.Tunnels[i].Address, req.Tunnels[i].Port, req.Tunnels[i].Name, id, localport)
		if err != nil {
			tx.Rollback()
			log.Errorf("CreateNewConnector error 4: %s", err.Error())
			return "", err
		}	
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewConnector error 5: %s", err.Error())
		return "", err
	}	

	return id, nil
}

func GetConnectors(schema string) ( []types.Connector, error) {
	var out []types.Connector //

    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, err := db.BeginTx(ctx, nil)
	sql := ` select a.id, a.name, a.accesskey, a.accesssecret, EXTRACT(epoch from (now() - a.createdat)), (select count(*) from `+schema+`.connections where connector_id=a.id) from `+schema+`.connectorauth as a;`
	rows, err  := tx.QueryContext(ctx, sql)
	if nil == err {
		defer rows.Close()
		for rows.Next() {
			var id, name, accesskey, accesssecret string 
			var nstatus int
			var age float64

			err = rows.Scan(&id, &name, &accesskey, &accesssecret, &age, &nstatus)
			if err != nil {
				tx.Rollback()
				log.Errorf("GetConnectors error 0: %s", err.Error())
				return out, err			
			}
			var o  types.Connector 
			o.Name = name 
			o.Id = id 
			o.Accesskey = &accesskey 
			if age > 60*60*24 {
				o.Secret = &pswd
			} else {
				o.Secret = &accesssecret
			}
			var st string
			if nstatus > 0 {
				st = "provisioned"
			} else {
				st = "configured"
			}
			t := types.TunnelStatus(st)
			o.Status = &t
			out = append(out, o)
		}

		for i:=0; i < len(out); i++ {
			sql := `select id, targetaddress, targetport, localport, connectorname, status from ` +schema+ 
			`.connectors where id_connectorauth=$1;`

			trows, err  := tx.QueryContext(ctx, sql, out[i].Id)
			if err != nil {
				tx.Rollback()
				log.Errorf("GetConnectors error 1: %s", err.Error())
				return out, err			
			}
			defer trows.Close()
			out[i].Tunnels = []types.Tunnel{}
			for trows.Next() {
				var id, targetaddress, targetport, localport, connectorname string 
				var status types.ConnectorStatus

				err = trows.Scan(&id, &targetaddress, &targetport, &localport, &connectorname, &status)
				if err != nil {
					tx.Rollback()
					log.Errorf("GetConnectors error 2: %s", err.Error())
					return out, err			
				}
				t := types.Tunnel{ &id, connectorname, targetaddress, targetport, &localport, &status }		
				out[i].Tunnels = append(out[i].Tunnels, t)		
			}			
		}
	} else {
		tx.Rollback()
		log.Errorf("GetConnectors error 0: %s", err.Error())
		return out, err
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("CreateNewConnector error 4: %s", err.Error())
		return out, err
	}	

	return out, nil
}

func UpdateConnector(schema string, connector *types.AddConnectorRequest) error {
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, err := db.BeginTx(ctx, nil)
	sql := `update ` + schema + `.connectorauth set name=$1 where id=$2;`

	_, err  = tx.ExecContext(ctx, sql, connector.Name, connector.Id)
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateConnector error 0: %s", err.Error())
		return err
	}	

	tunnelIds := []string{}
	// get all existing ids
	sql = `select id from ` + schema + `.connectors  where id_connectorauth=$1;`
	rows, err  := tx.QueryContext(ctx, sql, connector.Id)
	if nil == err {
		defer rows.Close()
		for rows.Next() {
			var id string
			err = rows.Scan(&id)
			if err != nil {
				tx.Rollback()
				log.Errorf("UpdateConnector error 1: %s", err.Error())
				return err
			}		
			tunnelIds = append(tunnelIds, id)		
		}
	} else {
		tx.Rollback()
		log.Errorf("UpdateConnector error 2: %s", err.Error())
		return err		
	}
	submitted := []string{}
	for i := 0; i < len(connector.Tunnels); i++ {
		submitted = append(submitted, *connector.Tunnels[i].Id)
	}

	missing := []string{}
	for i := 0; i < len(tunnelIds); i++ {
		hasif := contains(submitted, tunnelIds[i])
		if !hasif {
			missing = append(missing, tunnelIds[i])
		}
	}
	if len(missing) > 0 {
		sql = `delete from ` + schema + `.connectors where id=ANY($1);`
		_, err  = tx.ExecContext(ctx, sql,  pq.Array(missing))
		if err != nil {
			tx.Rollback()
			log.Errorf("UpdateConnector error 3: %s", err.Error())
			return err
		}			
	}

	for i := 0; i < len(connector.Tunnels); i++ {
		hasit := contains(tunnelIds, *connector.Tunnels[i].Id)

		if hasit {
			// update
			sql = `update `+schema+`.connectors set targetaddress=$1,targetport=$2,connectorname=$3 where id=$4;`
			_, err  = tx.ExecContext(ctx, sql, connector.Tunnels[i].Address, connector.Tunnels[i].Port, connector.Tunnels[i].Name, *connector.Tunnels[i].Id)
			if err != nil {
				tx.Rollback()
				log.Errorf("UpdateConnector error 4: %s", err.Error())
				return err
			}	
		} else {
			// insert
			ports, err := getports(schema, ctx, tx)

			if err != nil {
				tx.Rollback()
				log.Errorf("UpdateConnector error 5: %s", err.Error())
				return err
			}	

			localport, err := getport(ports)
			if err != nil {
				tx.Rollback()
				log.Errorf("UpdateConnector error 6: %s", err.Error())
				return err
			}	

			sql = `insert into `+schema+`.connectors(targetaddress,targetport,connectorname,id_connectorauth, localport) values($1,$2,$3,$4,$5) `
			_, err = tx.ExecContext(ctx, sql, connector.Tunnels[i].Address, connector.Tunnels[i].Port, connector.Tunnels[i].Name, connector.Id, localport)
			if err != nil {
				tx.Rollback()
				log.Errorf("UpdateConnector error 7: %s", err.Error())
				return err
			}				
		}
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("UpdateConnector error 8: %s", err.Error())
		return err
	}	
	return nil
}


func DeleteConnector(schema, Id string) error {
    ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancelfunc()

	tx, err := db.BeginTx(ctx, nil)
	sql := `delete from ` + schema + `.connectors  where id_connectorauth=$1;`

	_, err  = tx.ExecContext(ctx, sql, Id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnector error 0: %s", err.Error())
		return err
	}		

	sql = `delete from ` + schema + `.connectorauth  where id=$1;`

	_, err  = tx.ExecContext(ctx, sql, Id)
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnector error 1: %s", err.Error())
		return err
	}		

	err = tx.Commit()
	if err != nil {
		tx.Rollback()
		log.Errorf("DeleteConnector error 2: %s", err.Error())
		return err
	}	
	return nil
}

func CreateNewCustomer(customer types.Customer) error {
	sql := `insert into global.customers(company_name, schema_name, organization, domain, admin_group)
		values($1,$2,$3,$4,$5);`
	_, err := db.Exec(sql, customer.Name, customer.Schema, customer.Orgid, customer.Domain, customer.Admingroup)
	if err != nil {
		return err
	}
	var mallard string
	var where string
	if "" == os.Getenv("LOCAL_ENVIRONMENT") {
		// cloud, hence linux
		mallard = "./mallard"
		where = "/mallard"
	} else {
		// local Mac
		mallard = "../bin/mallard"
		where = "../../../dbConf"
	}
	dbhost := os.Getenv("DATABASE_HOST")
	dbpassword := os.Getenv("DATABASE_PASSWORD")
	dbport := os.Getenv("DATABASE_PORT")
	dbuser := os.Getenv("DATABASE_USER")
	dbname := os.Getenv("DATABASE_NAME")
	if dbport == "" {
		dbport = "5432"
	}
	if dbname == "" {
		dbname = "dymium"
	}
	cmd := exec.Command(mallard, "migrate", "-s", customer.Schema, "-r", "customer", 
		"--host", dbhost, 
		"--port", dbport, 
		"--user", dbuser, 
		"--password", dbpassword, 
		"--database", dbname,
		"--apply")

	fmt.Printf("Run %s in dir: %s, port: %s\n", mallard, where, dbport)		
	fmt.Printf("args: %v\n", cmd.Args)
	cmd.Dir = where
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr	
	err = cmd.Run()
	if err != nil {
		return errors.New( fmt.Sprintf("%s, mallard output: %s", err.Error(), stderr.String())  )
	} else {
		return err
	}
}
func GetCustomers() ([]types.Customer, error) {
	var ret []types.Customer
	sql := `select id, company_name, schema_name, organization, domain, admin_group from global.customers;`

	rows, err := db.Query(sql)
	if nil == err {
		defer rows.Close()

		for rows.Next() {
			var id, company_name, schema_name, organization, domain, admin_group string
			err = rows.Scan(&id, &company_name, &schema_name, &organization, &domain, &admin_group)
			if err != nil {
				break
			}
			c := types.Customer{&id, company_name, organization, schema_name, domain, admin_group}
			ret = append(ret, c)
		}
	}
	return ret, err
}

func DeleteCustomer(id string, schema string) error {
	sql := `delete from global.customers where id=$1;`
	_, err := db.Exec(sql, id)
	if err != nil {return err}

	var mallard, where string

	if "" == os.Getenv("LOCAL_ENVIRONMENT") {
		// cloud, hence linux
		mallard = "./mallard"
		where = "./mallard"
	} else {
		// local Mac
		mallard = "../bin/mallard"
		where = "../../../dbConf"
	}
	dbhost := os.Getenv("DATABASE_HOST")
	dbpassword := os.Getenv("DATABASE_PASSWORD")
	dbport := os.Getenv("DATABASE_PORT")
	dbuser := os.Getenv("DATABASE_USER")
	dbname := os.Getenv("DATABASE_NAME")
	if dbport == "" {
		dbport = "5432"
	}
	if dbname == "" {
		dbname = "dymium"
	}

	cmd := exec.Command(mallard, "drop", "-s", schema, 
		"--host", dbhost, 
		"--port", dbport, 
		"--user", dbuser, 
		"--password", dbpassword, 
		"--database", dbname)
	cmd.Dir = where		
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr	
	err = cmd.Run()
	if err != nil {
		return errors.New( fmt.Sprintf("%s, mallard output: %s", err.Error(), stderr.String())  )
	} else {
		return err
	}

}

func UpdateCustomer(customer types.Customer) error {
	sql := `update global.customers set organization=$1, domain=$2, admin_group=$3 where id=$4;`
	_, err := db.Exec(sql, customer.Orgid, customer.Domain, customer.Admingroup, customer.Id)
	if err != nil {return err}

	return err
}

func GetGlobalUsage() (types.GlobalUsage, error) {
	var usage types.GlobalUsage
	sql := `select count(*) from global.customers;`
	row := db.QueryRow(sql)

	err := row.Scan(&usage.Customers)
	
	bin, err := rdb.Get(ctx, "$:bytesin").Result ()
	usage.Bytesin = bin

	bout, err := rdb.Get(ctx, "$:bytesout").Result()
	usage.Bytesout = bout

	return usage, err
}
