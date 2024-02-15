package main

import (
	"encoding/json"
	"os"

	"database/sql"

	_ "github.com/lib/pq"

	"crypto/sha256"

	"fmt"

	"dymium.com/dymium/log"

	"aws"
        . "DbSetup"
	"DbSetup/types"
)

func main() {
	log.Init("DbGuardian")
	customerData, err := getCustomerData()
	if err != nil {
		log.Fatal(err.Error())
	}

	if err = createDatabases(customerData.Datascopes); err != nil {
		log.Fatal(err.Error())
	}

	connections := map[string]types.Connection{}
	for k := range customerData.Connections {
		c := &customerData.Connections[k]
		connections[c.Id] = *c
	}

	credentials := map[string]types.Credential{}
	for k := range customerData.Credentials {
		c := &customerData.Credentials[k]
		credentials[c.Connection_id] = *c
	}

	var user string
	if user = os.Getenv("DATABASE_USER"); user == "" {
		log.Fatal("Env var [DATABASE_USER] not defined")
		panic("Env var [DATABASE_USER] not defined")
	}

	for k := range customerData.Datascopes {
		datascope := &customerData.Datascopes[k]
		if db, err := sql.Open("postgres", fmt.Sprintf("host=/var/run/postgresql dbname='%s' user='%s' sslmode=disable",
			esc(datascope.Name), esc(user))); err != nil {
			log.Fatal(fmt.Sprintf("sql.Open(...,%s,%s): %v", LiteralEscape(datascope.Name), user, err))
		} else {
			defer db.Close()
			if err = ConfigureDatabase(db, datascope, connections, credentials, true); err != nil {
				log.Fatal(err.Error())
			}
		}
	}
}

func createDatabases(datascopes []types.Scope) error {

	var user, password string
	if user = os.Getenv("DATABASE_USER"); user == "" {
		return fmt.Errorf("Env var [DATABASE_USER] not defined")
	}
	if password = os.Getenv("DATABASE_PASSWORD"); password == "" {
		return fmt.Errorf("Env var [DATABASE_PASSWORD] not defined")
	}

	db, err := sql.Open("postgres", "host=/var/run/postgresql dbname=postgres user=postgres sslmode=disable")
	if err != nil {
		return fmt.Errorf("sql.Open: %+v", err)
	}
	defer db.Close()

	if _, err = db.Exec(fmt.Sprintf("CREATE USER %s SUPERUSER PASSWORD '"+esc(password)+"'", user)); err != nil {
		return err
	}

	for k := range datascopes {
		sql := fmt.Sprintf("CREATE DATABASE %s OWNER %s", LiteralEscape(datascopes[k].Name), user)
		if _, err = db.Exec(sql); err != nil {
			return err
		}
		log.Infof(sql)

		sql = fmt.Sprintf("REVOKE CONNECT ON DATABASE %s FROM PUBLIC", LiteralEscape(datascopes[k].Name))
		if _, err = db.Exec(sql); err != nil {
			return err
		}
		log.Info(sql)

		localUser := fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(datascopes[k].Name+"_dymium")))

		sql = fmt.Sprintf("CREATE ROLE %s", localUser)
		if _, err = db.Exec(sql); err != nil {
			return err
		}
		log.Info(sql)

		sql = fmt.Sprintf("GRANT CONNECT ON DATABASE %s TO %s", LiteralEscape(datascopes[k].Name), localUser)
		if _, err = db.Exec(sql); err != nil {
			return err
		}
		log.Info(sql)
	}

	return nil
}

func getCustomerData() (*types.CustomerData, error) {

	var customer string
	if customer = os.Getenv("CUSTOMER"); customer == "" {
		return nil, fmt.Errorf("Env var [CUSTOMER] not defined")
	}

	rq, err := json.Marshal(types.Request{
		Action:   types.A_Return,
		Customer: customer})
	if err != nil {
		return nil, fmt.Errorf("(impossible) Error in marshaling Request: %+v", err)
	}

	var cb []byte
	if cb, err = aws.Invoke("DbSync", nil, rq); err != nil {
		return nil, fmt.Errorf("request to DbSync returned error: %+v", err)
	}

	var c types.CustomerData
	if err := json.Unmarshal(cb, &c); err != nil {
		return nil, err
	}

	return &c, nil
}

func esc(str string) string {
        return ParamEscape(str)
}
