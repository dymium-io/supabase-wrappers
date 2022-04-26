/*
authentication
*/
package main

import (

	"database/sql"

	"fmt"
	_ "log"

	"strconv"


	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	_ "github.com/lib/pq"

)

type claims struct {
	ID         string   `json:"id"`
	Roles      []string `json:"roles"`
	CustomerId string   `json:"customerid"`
	jwt.StandardClaims
}
type adminClaims struct {
	AdminID string `json:"adminid"`
	jwt.StandardClaims
}

var psqlInfo string
var db *sql.DB

const timeOut = 20

func databaseInit(host string, password string, port string) {
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
}


func authenticationHandlers(p *mux.Router) error {

	return nil
}