//
// Copyright (c) 2022 Dymium, Inc. All rights reserved.
// written by igor@dymium.io
//
package dhandlers

import "dymium.com/dymium/authentication"
import "dymium.com/dymium/types"
import "os"
import "os/exec"
import "testing"
import "path/filepath"
import "net/http"
import "fmt"
import "net/http/httptest"
import "bytes"
import "io/ioutil"
import "encoding/json"
import "github.com/stretchr/testify/assert"

var status types.OperationStatus
var adventureworks = `{"name":"adventureworks","dbtype":"postgres","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"username":"postgres","password":"$kdvnMsp4o","description":"test data base from Microsoft"}`
var northwind = `{"name":"northwind","dbtype":"postgres","address":"docker.for.mac.host.internal","port":5432,"dbname":"northwind","useTLS":false,"username":"postgres","password":"$kdvnMsp4o","description":"another MS database"}`
func TestApiHandlers(t *testing.T){
///------------------ db setup start ------------------------	
	func () {
		dbhost := os.Getenv("DATABASE_HOST")
		dbpassword := os.Getenv("DATABASE_PASSWORD")
		dbport := os.Getenv("DATABASE_PORT")
		dbuser := os.Getenv("DATABASE_USER")
		dbadminuser := os.Getenv("DATABASE_ADMIN_USER")

		dbtls  := os.Getenv("DATABASE_TLS")
		if(dbtls == "")	{
			dbtls = "disable"
		}
		err := authentication.DatabaseInit(dbhost, dbport, dbadminuser, dbpassword, "postgres", dbtls)
		if(err != nil) {
			t.Errorf("Error: %s\n", err.Error() )
			return
		}
		db := authentication.GetDB()

		sql := `drop database  if exists test ;`
		_, err =  db.Exec(sql)
		if(err != nil) {
			t.Errorf("Error dropping database: %s\n", err.Error() )
			return
		}

		sql = `create database test owner `+dbuser+`;`
		_, err =  db.Exec(sql)
		if(err != nil) {
			t.Errorf("Error creating database: %s\n", err.Error() )
			return
		}

		sql = `grant all on database test to `+dbuser+`;`
		_, err =  db.Exec(sql)
		if(err != nil) {
			t.Errorf("Error grantin all on database: %s\n", err.Error() )
			return
		}
		err = db.Close()

		// ----------------------------- open test -----------------------------------
		err = authentication.DatabaseInit(dbhost, dbport, dbadminuser, dbpassword, "test", dbtls)
		if(err != nil) {
			t.Errorf("Error: %s\n", err.Error() )
			return
		}
		db = authentication.GetDB()
		sql = `CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public`
		_, err =  db.Exec( sql)
		if(err != nil) {
			t.Errorf("Error creating extension: %s\n", err.Error() )
			return
		}

		exe, err := filepath.Abs("../../../../bin/mallard") 
		if err != nil {
			t.Errorf("Error resolving mallard: %s\n", err.Error())
			return
		}	
		path, err := filepath.Abs("../../../../DbConf/public")
		if err != nil {
			t.Errorf("Error resolving public: %s\n", err.Error())
			return
		}	

		path, err = filepath.Abs("../../../../DbConf/global")
		if err != nil {
			t.Errorf("Error resolving global: %s\n", err.Error())
			return
		}

		cmd := exec.Command(exe, "migrate", "-s", "global", "-r", path, "--host", dbhost, "--port", dbport, 
		"--user", dbuser, "--database", "test", "--create-schema", "--apply")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		err = cmd.Run()

		if err != nil {
			t.Errorf("\nError running mallard:\n%s\n %s\n", cmd, err.Error())
			return
		}	

		path, err = filepath.Abs("../../../../DbConf/customer")
		if err != nil {
			t.Errorf("Error resolving customer: %s\n", err.Error())
			return
		}

		cmd = exec.Command(exe, "migrate", "-s", "spoofcorp", "-r", path, "--host", dbhost, "--port", dbport, 
			"--user", dbuser, "--database", "test", "--create-schema", "--apply")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		err = cmd.Run()
		
		if err != nil {
			t.Errorf("Error running mallard:\n%s\n %s\n", cmd, err.Error())
			return
		}	

		sql = `insert into global.customers(company_name, schema_name, organization, domain) 
		values($1,$2,$3,$4) returning id; `

		_, err =  db.Exec( sql, "Spoof Corporation", "spoofcorp", "org_nsEsSgfq3IYXe2pu", "spoofcorp.com")
		if err != nil {
			t.Errorf("Error creating customer: %s\n", err.Error())
			return
		}	
	}()
///------------------ db setup done ------------------------

//------------------- get JWT ------------------------
	token, err :=  authentication.GeneratePortalJWT("https://media-exp2.licdn.com/dms/image/C5603AQGQMJOel6FJxw/profile-displayphoto-shrink_400_400/0/1570405959680?e=1661385600&v=beta&t=MDpCTJzRSVtovAHXSSnw19D8Tr1eM2hmB0JB63yLb1s", 
	"spoofcorp", "org_nsEsSgfq3IYXe2pu")
	if(err != nil){
		t.Errorf("Error creating token: %s\n", err.Error())
		return		
	}

	//---- test static file	
	func () {
		req, err := http.NewRequest("GET", "/logo.png", nil)
		if err != nil {
			t.Fatal(err)
		}
		rr := httptest.NewRecorder()
		handler := http.HandlerFunc(GetImages)
		handler.ServeHTTP(rr, req)
		// Check the status code is what we expect.
		if status := rr.Code; status != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				status, http.StatusOK)
			return
		}
		fmt.Println("Static file retrieved correctly")
	}()
//---- test missing file
	func () {
		req, err := http.NewRequest("GET", "/xxxxx.yy", nil)
		if err != nil {
			t.Fatal(err)
		}
		req.Header.Set("Authorization", "Bearer "+token)
		rr := httptest.NewRecorder()
		handler := http.HandlerFunc(GetImages)
		handler.ServeHTTP(rr, req)
		// Check the status code is what we expect.
		if status := rr.Code; status != http.StatusNotFound {
			t.Errorf("handler returned wrong status code: got %v want %v",
				status, http.StatusNotFound)
			return
		}	
		fmt.Println("No static file retrieved 404 correctly")
	}()
// -------- test auth middleware	
	func() {
		req, err := http.NewRequest("POST", "/api/createnewconnection",  bytes.NewBuffer([]byte(northwind )))
		if err != nil {
			t.Fatal(err)
		}
		//req.Header.Set("Authorization", "Bearer "+token)
		req.Header.Set("Content-Type", "application/json")
		rr := httptest.NewRecorder()

		h := http.HandlerFunc(CreateNewConnection)
		hh := AuthMiddleware(h)
		hh.ServeHTTP(rr, req)
		body, _ := ioutil.ReadAll(rr.Body)
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return
		}
		err = json.Unmarshal(body, &status)
		if err != nil {
			t.Errorf("Unmarshaling error: %s\n", err.Error() )
			return
		}
		if(status.Status == "OK") {
			t.Errorf("Authentication should not have passed\n" )
			return		
		}
		fmt.Println("Authentication without JWT failed correctly")
	}()
	// -------- create a connection	
	createConnection := func(data string) {
		req, err := http.NewRequest("POST", "/api/createnewconnection",  bytes.NewBuffer([]byte(data )))
		if err != nil {
			t.Fatal(err)
		}
		req.Header.Set("Authorization", "Bearer "+token)
		req.Header.Set("Content-Type", "application/json")
		rr := httptest.NewRecorder()

		h := http.HandlerFunc(CreateNewConnection)
		hh := AuthMiddleware(h)
		hh.ServeHTTP(rr, req)
		body, _ := ioutil.ReadAll(rr.Body)
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return
		}
		err = json.Unmarshal(body, &status)
		if err != nil {
			t.Errorf("Unmarshaling error: %s\n", err.Error() )
			return
		}
		if(status.Status != "OK") {
			t.Errorf("Authentication should not have passed\n" )
			return		
		}
		fmt.Println("Created connection, should check the database")
	}
	createConnection(adventureworks)
	countConnections := func (c int) {
		db := authentication.GetDB()
		sql := `select count(*) from spoofcorp.connections;`
		row := db.QueryRow(sql)
		var count int 
		row.Scan(&count)
		fmt.Printf("Number of connections: %d\n", count)
		assert.Equal(t, count, c, "One record should be present")
	}
	countConnections(1)
	createConnection(northwind)
	countConnections(2)
	// -------- create a connection	
	createDupConnection := func(data string) {
		req, err := http.NewRequest("POST", "/api/createnewconnection",  bytes.NewBuffer([]byte(data )))
		if err != nil {
			t.Fatal(err)
		}
		req.Header.Set("Authorization", "Bearer "+token)
		req.Header.Set("Content-Type", "application/json")
		rr := httptest.NewRecorder()

		h := http.HandlerFunc(CreateNewConnection)
		hh := AuthMiddleware(h)
		hh.ServeHTTP(rr, req)
		body, _ := ioutil.ReadAll(rr.Body)
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return
		}
		err = json.Unmarshal(body, &status)
		if err != nil {
			t.Errorf("Unmarshaling error: %s\n", err.Error() )
			return
		}
		if(status.Status == "OK") {
			t.Errorf("The insertion of duplicate name in connection should have failed\n" )
			return		
		}
		fmt.Printf("Test properly failed with the error: %s\n", status.Errormessage)
	}
	createDupConnection(northwind)

	
	func () {
		req, err := http.NewRequest("GET", "/api/getconnections",  nil)
		if err != nil {
			t.Fatal(err)
		}
		req.Header.Set("Authorization", "Bearer "+token)
		req.Header.Set("Content-Type", "application/json")
		rr := httptest.NewRecorder()

		h := http.HandlerFunc(GetConnections)
		hh := AuthMiddleware(h)
		hh.ServeHTTP(rr, req)
		body, _ := ioutil.ReadAll(rr.Body)
		fmt.Printf("%s\n", string(body))
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return
		}
		err = json.Unmarshal(body, &status)
		if err != nil {
			t.Errorf("Unmarshaling error: %s\n", err.Error() )
			return
		}
		if(status.Status != "OK") {
			t.Errorf("Authentication should not have passed\n" )
			return		
		}
		fmt.Println("Created connection, should check the database")			
	}()
}
