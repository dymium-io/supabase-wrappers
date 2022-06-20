package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"log"
)

func doDelete(datascope string, cnf *guardianConf) (empty struct{}, err error) {
	sslmode_ := "disable"
	if *cnf.GuardianTls {
		sslmode_ = "require"
	}

	connectStr := fmt.Sprintf("host=%%s port=%d dbname='%s' user=%s password='%s' sslmode=%s",
		cnf.GuardianPort, cnf.GuardianDatabase, cnf.GuardianUser, cnf.GuardianAdminPassword, sslmode_)

	for _, a := range cnf.GuardianAddress {
		if db, err := sql.Open("postgres", fmt.Sprintf(connectStr, a)); err != nil {
			log.Printf("Cannot open connection to %s. Ignoring error: %v", a, err)
		} else {
			defer db.Close()
			if _, err = db.Exec(fmt.Sprintf("DROP DATABASE IF EXISTS %q", datascope)); err != nil {
				log.Printf("Cannot drop database %q at %s. Ignoring error: %v",
					datascope, a, err)				
			}
		}
	}

	return empty, nil
}
