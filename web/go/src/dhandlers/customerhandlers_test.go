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
import "io"
import "net/http/httptest"
import "bytes"
import "io/ioutil"
import "encoding/json"
import "github.com/stretchr/testify/assert"
import "github.com/stretchr/testify/require"

var status types.OperationStatus
var adventureworks = `{"name":"adventureworks","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"username":"postgres","password":"$kdvnMsp4o","description":"test data base from Microsoft"}`
var northwind = `{"name":"northwind","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"northwind","useTLS":false,"username":"postgres","password":"$kdvnMsp4o","description":"another MS database"}`
var todelete = `{"name":"todelete","dbtype":"PostgreSQL","address":"deletemehost","port":5432,"dbname":"todelete","useTLS":false,"username":"postgres","password":"rqweqwrqweqweqw","description":"database to delete"}`

var datascope_test_1 = `{"name":"test1","id":"","records":[{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"integer","position":1,"reference":null,"action":"Obfuscate","col":"jobcandidateid","semantics":"Job position","dflt":"nextval('humanresources.jobcandidate_jobcandidateid_seq'::regclass)","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"integer","position":2,"reference":{"schema":"humanresources","table":"employee","column":"businessentityid"},"action":"Obfuscate","col":"businessentityid","semantics":"Business entity Id","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"xml","position":3,"reference":null,"action":"Allow","col":"resume","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"timestamp without time zone","position":4,"reference":null,"action":"Allow","col":"modifieddate","semantics":"N/A","dflt":"now()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"integer","position":1,"reference":null,"action":"Obfuscate","col":"businessentityid","semantics":"Business entity Id","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(15)","position":2,"reference":null,"action":"Obfuscate","col":"nationalidnumber","semantics":"Social Security Number","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(256)","position":3,"reference":null,"action":"Block","col":"loginid","semantics":"Login details","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(50)","position":6,"reference":null,"action":"Obfuscate","col":"jobtitle","semantics":"Job position","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"date","position":7,"reference":null,"action":"Allow","col":"birthdate","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"character(1)","position":8,"reference":null,"action":"Allow","col":"maritalstatus","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"character(1)","position":9,"reference":null,"action":"Obfuscate","col":"gender","semantics":"Gender","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"date","position":10,"reference":null,"action":"Allow","col":"hiredate","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"boolean","position":11,"reference":null,"action":"Allow","col":"salariedflag","semantics":"N/A","dflt":"true","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"smallint","position":12,"reference":null,"action":"Allow","col":"vacationhours","semantics":"N/A","dflt":"0","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"smallint","position":13,"reference":null,"action":"Allow","col":"sickleavehours","semantics":"N/A","dflt":"0","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"boolean","position":14,"reference":null,"action":"Allow","col":"currentflag","semantics":"N/A","dflt":"true","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"uuid","position":15,"reference":null,"action":"Allow","col":"rowguid","semantics":"N/A","dflt":"uuid_generate_v1()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"timestamp without time zone","position":16,"reference":null,"action":"Allow","col":"modifieddate","semantics":"N/A","dflt":"now()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar","position":17,"reference":null,"action":"Allow","col":"organizationnode","semantics":"N/A","dflt":"'/'::character varying","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"smallint","position":1,"reference":null,"action":"Allow","col":"employee_id","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(20)","position":2,"reference":null,"action":"Obfuscate","col":"last_name","semantics":"Last name","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(10)","position":3,"reference":null,"action":"Obfuscate","col":"first_name","semantics":"First name","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(30)","position":4,"reference":null,"action":"Allow","col":"title","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(25)","position":5,"reference":null,"action":"Allow","col":"title_of_courtesy","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"date","position":6,"reference":null,"action":"Allow","col":"birth_date","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"date","position":7,"reference":null,"action":"Allow","col":"hire_date","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(60)","position":8,"reference":null,"action":"Obfuscate","col":"address","semantics":"Address","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":9,"reference":null,"action":"Obfuscate","col":"city","semantics":"City","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":10,"reference":null,"action":"Allow","col":"region","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(10)","position":11,"reference":null,"action":"Obfuscate","col":"postal_code","semantics":"Zipcode","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":12,"reference":null,"action":"Obfuscate","col":"country","semantics":"Country","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(24)","position":13,"reference":null,"action":"Obfuscate","col":"home_phone","semantics":"Telephone number","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(4)","position":14,"reference":null,"action":"Allow","col":"extension","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"bytea","position":15,"reference":null,"action":"Allow","col":"photo","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"text","position":16,"reference":null,"action":"Allow","col":"notes","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"smallint","position":17,"reference":{"schema":"public","table":"employees","column":"employee_id"},"action":"Allow","col":"reports_to","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(255)","position":18,"reference":null,"action":"Allow","col":"photo_path","semantics":"N/A","dflt":"","isnullable":true}]}`
var datascope_test_2 = `{"name":"test2","id":"","records":[{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"integer","position":1,"reference":null,"action":"Obfuscate","col":"addressid","semantics":"Address","dflt":"nextval('person.address_addressid_seq'::regclass)","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"varchar(60)","position":2,"reference":null,"action":"Obfuscate","col":"addressline1","semantics":"Address","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"varchar(60)","position":3,"reference":null,"action":"Obfuscate","col":"addressline2","semantics":"Address","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"varchar(30)","position":4,"reference":null,"action":"Obfuscate","col":"city","semantics":"City","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"integer","position":5,"reference":{"schema":"person","table":"stateprovince","column":"stateprovinceid"},"action":"Obfuscate","col":"stateprovinceid","semantics":"State","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"varchar(15)","position":6,"reference":null,"action":"Obfuscate","col":"postalcode","semantics":"Zipcode","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"varchar(44)","position":7,"reference":null,"action":"Allow","col":"spatiallocation","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"uuid","position":8,"reference":null,"action":"Allow","col":"rowguid","semantics":"N/A","dflt":"uuid_generate_v1()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"person","table":"address","typ":"timestamp without time zone","position":9,"reference":null,"action":"Allow","col":"modifieddate","semantics":"N/A","dflt":"now()","isnullable":false}]}`

var datascope_update_test_1 = `{"name":"test1","id":"","records":[{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"integer","position":1,"reference":null,"action":"Obfuscate","col":"jobcandidateid","semantics":"Job position","dflt":"nextval('humanresources.jobcandidate_jobcandidateid_seq'::regclass)","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"integer","position":2,"reference":{"schema":"humanresources","table":"employee","column":"businessentityid"},"action":"Obfuscate","col":"businessentityid","semantics":"Business entity Id","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"xml","position":3,"reference":null,"action":"Allow","col":"resume","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"jobcandidate","typ":"timestamp without time zone","position":4,"reference":null,"action":"Allow","col":"modifieddate","semantics":"N/A","dflt":"now()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"integer","position":1,"reference":null,"action":"Obfuscate","col":"businessentityid","semantics":"Business entity Id","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(15)","position":2,"reference":null,"action":"Obfuscate","col":"nationalidnumber","semantics":"Social Security Number","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(256)","position":3,"reference":null,"action":"Block","col":"loginid","semantics":"Login details","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar(50)","position":6,"reference":null,"action":"Obfuscate","col":"jobtitle","semantics":"Job position","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"date","position":7,"reference":null,"action":"Allow","col":"birthdate","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"character(1)","position":8,"reference":null,"action":"Allow","col":"maritalstatus","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"character(1)","position":9,"reference":null,"action":"Obfuscate","col":"gender","semantics":"Gender","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"date","position":10,"reference":null,"action":"Allow","col":"hiredate","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"boolean","position":11,"reference":null,"action":"Allow","col":"salariedflag","semantics":"N/A","dflt":"true","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"smallint","position":12,"reference":null,"action":"Allow","col":"vacationhours","semantics":"N/A","dflt":"0","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"smallint","position":13,"reference":null,"action":"Allow","col":"sickleavehours","semantics":"N/A","dflt":"0","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"boolean","position":14,"reference":null,"action":"Allow","col":"currentflag","semantics":"N/A","dflt":"true","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"uuid","position":15,"reference":null,"action":"Allow","col":"rowguid","semantics":"N/A","dflt":"uuid_generate_v1()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"timestamp without time zone","position":16,"reference":null,"action":"Allow","col":"modifieddate","semantics":"N/A","dflt":"now()","isnullable":false},{"id":null,"connection":"adventureworks","connectionId":null,"schema":"humanresources","table":"employee","typ":"varchar","position":17,"reference":null,"action":"Allow","col":"organizationnode","semantics":"N/A","dflt":"'/'::character varying","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"smallint","position":1,"reference":null,"action":"Allow","col":"employee_id","semantics":"N/A","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(20)","position":2,"reference":null,"action":"Obfuscate","col":"last_name","semantics":"Last name","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(10)","position":3,"reference":null,"action":"Obfuscate","col":"first_name","semantics":"First name","dflt":"","isnullable":false},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(30)","position":4,"reference":null,"action":"Allow","col":"title","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(25)","position":5,"reference":null,"action":"Allow","col":"title_of_courtesy","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"date","position":6,"reference":null,"action":"Allow","col":"birth_date","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"date","position":7,"reference":null,"action":"Allow","col":"hire_date","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(60)","position":8,"reference":null,"action":"Obfuscate","col":"address","semantics":"Address","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":9,"reference":null,"action":"Obfuscate","col":"city","semantics":"City","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":10,"reference":null,"action":"Allow","col":"region","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(10)","position":11,"reference":null,"action":"Obfuscate","col":"postal_code","semantics":"Zipcode","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(15)","position":12,"reference":null,"action":"Obfuscate","col":"country","semantics":"Country","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(24)","position":13,"reference":null,"action":"Obfuscate","col":"home_phone","semantics":"Telephone number","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(4)","position":14,"reference":null,"action":"Allow","col":"extension","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"bytea","position":15,"reference":null,"action":"Allow","col":"photo","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"text","position":16,"reference":null,"action":"Allow","col":"notes","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"smallint","position":17,"reference":{"schema":"public","table":"employees","column":"employee_id"},"action":"Allow","col":"reports_to","semantics":"N/A","dflt":"","isnullable":true},{"id":null,"connection":"northwind","connectionId":null,"schema":"public","table":"employees","typ":"varchar(255)","position":18,"reference":null,"action":"Allow","col":"photo_path","semantics":"N/A","dflt":"","isnullable":true}]}`

var edited_adventureworks = `{"Id":"d057ee99-ff26-414a-9a46-467420cdb932","Name":"adventureworks","DbType":"PostgreSQL","Address":"docker.for.mac.host.internal","Port":5432,"Dbname":"Adventureworks","Description":"edited test data base from Microsoft"}`
var edited_northwind = `{"Id":"57510719-2ca7-49e7-92bf-d174cf8f0e08","Name":"northwind","DbType":"PostgreSQL","Address":"docker.for.mac.host.internal","Port":5432,"Dbname":"northwind","Description":"another MS database edited with password","Username":"newusername","Password":"newpassword"}`

var groupmapping_1 = `{"dymiumgroup":"dada","directorygroup":"production","comments":"test"}`
var groupmapping_2 = `{"dymiumgroup":"admins","directorygroup":"admins","comments":"Admin group test"}`
var groupmapping_3 = `{"dymiumgroup":"foo","directorygroup":"bar","comments":"Admin group fubar"}`

var savegroups_1 = `{"name":"test1", "groups":[{"id":"ae5631a4-5947-42e7-8db9-22b5d2d6f22a","name":"foo"},{"id":"54688195-e574-462d-9c1b-267e9695a178","name":"dada"}]}`
var savegroups_2 = `{"name":"test2","groups":[{"id":"ae5631a4-5947-42e7-8db9-22b5d2d6f22a","name":"foo"}]}`

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
		err := authentication.Init(dbhost, dbport, dbadminuser, dbpassword, "postgres", dbtls)
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
		err = authentication.Init(dbhost, dbport, dbadminuser, dbpassword, "test", dbtls)
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
	authentication.InitInvoke( func(a string, b *string, data []byte)([]byte, error) {
		fmt.Println("Our InitInvoke called")
		return []byte(`{}`), nil
	}) 
	// -------- create a connection	
	LoadAuthHandler := func (method string, url string, reader io.Reader, 
		handler func(http.ResponseWriter, *http.Request))  ( *httptest.ResponseRecorder, []byte) {

		req, err := http.NewRequest(method, url, reader)
		if err != nil {
			t.Fatal(err)
		}
		req.Header.Set("Authorization", "Bearer "+token)
		req.Header.Set("Content-Type", "application/json")
		rr := httptest.NewRecorder()

		h := http.HandlerFunc(handler)
		hh := AuthMiddleware(h)
		hh.ServeHTTP(rr, req)
		body, _ := ioutil.ReadAll(rr.Body)

		return rr, body
	}	
	createConnection := func(data string) {

		rr, body := LoadAuthHandler("POST", "/api/createnewconnection", bytes.NewBuffer([]byte(data )), 
			CreateNewConnection) 

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
			t.Errorf("Authentication should have passed %s\n", status.Errormessage )
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

		rr, body := LoadAuthHandler("POST", "/api/createnewconnection", bytes.NewBuffer([]byte(data )), 
			CreateNewConnection) 

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
	createConnection(todelete)
	countConnections(3)
	
	getConnections := func (expected int) (string, []types.ConnectionRecord) {

		rr, body := LoadAuthHandler("GET", "/api/getconnections", nil,
			GetConnections) 
		
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return "", []types.ConnectionRecord{}
		}
		var status types.ConnectionResponse
		err = json.Unmarshal(body, &status)
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in GetConnections %s\n", status.Errormessage ) )
		require.Equal(t, expected, len(status.Data), "Three records should be present")
		return *status.Data[ len(status.Data ) -1 ].Id, status.Data
	}
	id, _ := getConnections(3)
	
	func (id string)  {
		cdr := types.ConnectionDetailRequest{id}
		js, _ := json.Marshal(cdr)

		rr, body := LoadAuthHandler("POST", "/api/queryconnection", bytes.NewBuffer(js), 
			QueryConnection) 
		
		require.Equal(t, nil, err, fmt.Errorf("Error reading body: %s\n", err ) )

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}
		var status  types.ConnectionDetailResponse
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in DeleteConnection %s\n", status.Errormessage ) )

		return
	}(id)

	func (id string)  {
		cdr := types.ConnectionDetailRequest{id}
		js, _ := json.Marshal(cdr)

		rr, body := LoadAuthHandler("POST", "/api/queryconnection", bytes.NewBuffer(js), 
			QueryConnection) 
		
		require.Equal(t, nil, err, fmt.Errorf("Error reading body: %s\n", err ) )

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}
		var status  types.ConnectionDetailResponse
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in Queryonnection %s\n", status.Errormessage ) )

		return
	}(id)


	func (id string)  {
		fmt.Printf("Delete connection %s\n", id)
		var ds types.DatascopeId = types.DatascopeId{Id:id}
		js, err := json.Marshal(ds)
		require.Equal(t, nil, err, fmt.Errorf("Error marshaling: %s\n", err ) )

		rr, body := LoadAuthHandler("POST", "/api/deleteconnection", bytes.NewBuffer(js), 
			DeleteConnection) 
		
		require.Equal(t, nil, err, fmt.Errorf("Error reading body: %s\n", err ) )

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in DeleteConnection %s\n", status.Errormessage ) )

		return
	}(id)

	id, _ = getConnections(2)
// -------- edit a connection without password
	func (data string) {
		rr, body := LoadAuthHandler("POST", "/api/updateconnection", bytes.NewBuffer([]byte(data)), 
			UpdateConnection) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
		fmt.Println("Check update properly failing")
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.NotEqual(t, "OK", status.Status, fmt.Sprintf("Error in UpdateConnection %s\n", status.Errormessage ) )

	}(edited_adventureworks)
	UpdateConn := func (data, name string) {
		_, conns := getConnections(2)
		var realid string
		for i := 0; i < len(conns); i++ {
			if(conns[i].Name == name) {
				realid = *conns[i].Id
			}
		}
		fmt.Printf("determined id as %s\n", realid)
		var cr types.ConnectionRecord 
		json.Unmarshal([]byte(data), &cr)
		*cr.Id = realid 
		js, _ := json.Marshal(cr)

		rr, body := LoadAuthHandler("POST", "/api/updateconnection", bytes.NewBuffer([]byte(js)), 
			UpdateConnection) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in UpdateConnection %s\n", status.Errormessage ) )
	
	}
	UpdateConn(edited_adventureworks, "adventureworks")
	UpdateConn(edited_northwind, "northwind")

	// now let' start with datascopes
	SaveDs := func (data string) {
		rr, body := LoadAuthHandler("POST", "/api/savedatascope", bytes.NewBuffer([]byte(data)), 
			SaveDatascope) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
		
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in SaveDatascope %s\n", status.Errormessage ) )

	}
	SaveDs(datascope_test_1)
	func (data string) {
		fmt.Printf("Save datascope")
		rr, body := LoadAuthHandler("POST", "/api/savedatascope", bytes.NewBuffer([]byte(data)), 
			SaveDatascope) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.NotEqual(t, "OK", status.Status, fmt.Sprintf("Error in SaveDatascope %s\n", status.Errormessage ) )
		fmt.Println("SaveDatascope properly failing")
	}(datascope_test_1)

	/*
	authentication.InitInvoke( func(a string, b *string, data []byte)([]byte, error) {
		fmt.Println("Our InitInvoke called")
		return []byte(`{"Errormessage":"Zhopa"}`), nil
	}) 
    */


	SaveDs(datascope_test_2)

	//type Invoke_t func(string, *string, []byte) ([]byte, error)
	authentication.InitInvoke( func(a string, b *string, data []byte)([]byte, error) {
		return []byte(`{}`), nil
	}) 	
	idnames := func () []types.DatascopeIdName {
	
		rr, body := LoadAuthHandler("GET", "/api/getdatascopes", nil, 
			GetDatascopes) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return nil
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)
	
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in GetDatascopes %s\n", status.Errormessage ) )
		require.Equal(t, 2, len(status.Records), "Error: the number of datascopes must be 2!"  )
		return status.Records
	}()	

	func () {
		fmt.Printf("Get datascope details")
		
		id := types.DatascopeId{idnames[0].Id }
		js, _ := json.Marshal(id)

		rr, body := LoadAuthHandler("POST", "/api/getdatascopedetails", bytes.NewBuffer([]byte(js)), 
			GetDatascopeDetails) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in GetDatascopeDetails %s\n", status.Errormessage ) )

	}()	


	UpdateDs := func (data string) {
		rr, body := LoadAuthHandler("POST", "/api/updatedatascope", bytes.NewBuffer([]byte(data)), 
			UpdateDatascope) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		var status  types.OperationStatus
		err = json.Unmarshal(body, &status)
		
		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in SaveDatascope %s\n", status.Errormessage ) )

	}	
	UpdateDs(datascope_update_test_1)
	AddGroup := func (data string) {
		rr, body := LoadAuthHandler("POST", "/api/createmapping", bytes.NewBuffer([]byte(data)), 
			CreateMapping) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in CreateMapping %s\n", status.Errormessage ) )

	}	
	AddGroup(groupmapping_1)
	AddGroup(groupmapping_2)


	func () {
		rr, body := LoadAuthHandler("POST", "/api/createmapping", bytes.NewBuffer([]byte(groupmapping_1)), 
			CreateMapping) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.NotEqual(t, "OK", status.Status, fmt.Sprintf("Error in CreateMapping %s\n", status.Errormessage ) )
		fmt.Println("Proper error is thrown for CreateMapping")
	}()

	AddGroup(groupmapping_3)
		
	func () string {
		rr, body := LoadAuthHandler("GET", "/api/updatemapping", nil, GetMappings) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return ""
		}		
	
		var status  types.GroupMappingStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in CreateMapping %s\n", status.Errormessage ) )

		record := status.Records[0]
		record.Comments = "Comment changed via test"
		js, _ := json.Marshal(record)

		rr, body = LoadAuthHandler("POST", "/api/updatemapping", bytes.NewBuffer([]byte(js)), 
			UpdateMapping) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return ""
		}		
		
		var ostatus  types.DatascopesStatus
		err = json.Unmarshal(body, &ostatus)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", ostatus.Status, fmt.Sprintf("Error in UpdateMapping %s\n", ostatus.Errormessage ) )

		return *status.Records[0].Id
	}()		
	

	id = func () string {
		rr, body := LoadAuthHandler("GET", "/api/getmappings", nil, GetMappings) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return ""
		}		
	
		var status  types.GroupMappingStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in CreateMapping %s\n", status.Errormessage ) )
		return *status.Records[0].Id
	}()	

	func (id string) {
		r := types.RequestById{id}
		js, _ := json.Marshal(r)
		rr, body := LoadAuthHandler("POST", "/api/deletemapping", bytes.NewBuffer([]byte(js)), 
			DeleteMapping) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in DeleteMapping %s\n", status.Errormessage ) )

	}	(id)

	func () string {
		rr, body := LoadAuthHandler("GET", "/api/getmappings", nil, GetMappings) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return ""
		}		
	
		var status  types.GroupMappingStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in DeleteMapping %s\n", status.Errormessage ) )
		require.Equal(t, 2, len(status.Records), fmt.Sprintf("Error in DeleteMapping, wrong number of recordds left \n" ) )
		return *status.Records[0].Id
	}()	

	
	AddGroupDatascopeMapping := func() {
		rr, body := LoadAuthHandler("GET", "/api/getmappings", nil, GetMappings) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		var mapping  types.GroupMappingStatus
		err = json.Unmarshal(body, &mapping)

		rr, body = LoadAuthHandler("GET", "/api/getdatascopes", nil, 
			GetDatascopes) 
		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var datascopes  types.DatascopesStatus
		err = json.Unmarshal(body, &datascopes)

		var ga types.GroupAssignment
		ga.Id = datascopes.Records[0].Id
		ga.Name = datascopes.Records[0].Name
		ga.Groups = append(ga.Groups, types.DatascopeIdName { mapping.Records[0].Dymiumgroup, *mapping.Records[0].Id})

		js, _ := json.Marshal(ga)
fmt.Printf("send: %s\n", string(js))
		rr, body = LoadAuthHandler("POST", "/api/savegroups", bytes.NewBuffer([]byte(js)), 
			SaveGroups) 

		if s := rr.Code; s != http.StatusOK {
			t.Errorf("handler returned wrong status code: got %v want %v",
				s, http.StatusOK)
			return 
		}		
		
		var status  types.DatascopesStatus
		err = json.Unmarshal(body, &status)

		require.Equal(t, nil, err, fmt.Errorf("Unmarshaling error: %s\n", err ) )
		require.Equal(t, "OK", status.Status, fmt.Sprintf("Error in CreateMapping %s\n", status.Errormessage ) )

	}	
	AddGroupDatascopeMapping()
}


