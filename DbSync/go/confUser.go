package main

import (
	"database/sql"
	"strings"

	_ "github.com/lib/pq"

	"fmt"
	"log"
	"sort"

	"crypto/sha256"

        . "DbSetup"
	"DbSetup/types"
)

func confUser(
	userCnf *types.UserConf,
	cnf *guardianConf,
) (empty struct{}, err error) {

	if userCnf == nil {
		return empty, fmt.Errorf("User conf is not defined")
	}

	// log.Printf("confUser: { Name: %s, Datascopes: %v }\n",
	// userCnf.Name, userCnf.Datascopes)
	// log.Printf("guardianCnf: { Address: %v:%d, Database: %s}\n",
	//	cnf.GuardianAddress, cnf.GuardianPort, cnf.GuardianDatabase)

	sslmode_ := "disable"
	if *cnf.GuardianTls {
		sslmode_ = "require"
	}

	type udb struct{ u, db string }
	ds := make([]udb, len(userCnf.Datascopes))
	for k := range userCnf.Datascopes {
		ds[k] = udb{
			u:  fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(userCnf.Datascopes[k]+"_dymium"))),
			db: userCnf.Datascopes[k],
		}
	}
	sort.Slice(ds, func(i, j int) bool { return ds[i].u < ds[j].u })

	connectStr := fmt.Sprintf("host=%%s port=%d dbname='%s' user=%s password='%s' sslmode=%s",
		cnf.GuardianPort, cnf.GuardianDatabase, cnf.GuardianUser, cnf.GuardianAdminPassword, sslmode_)

	for _, a := range cnf.GuardianAddress {
		if db, err := sql.Open("postgres", fmt.Sprintf(connectStr, a)); err != nil {
			log.Printf("Cannot open connection to %s. Ignoring error: %v", a, err)
		} else {
			defer db.Close()
			userExists := false
			escName := PostgresEscape(userCnf.Name)
			if rows, err := db.Query(`SELECT true FROM pg_roles WHERE LOWER(oid::regrole::text) = LOWER($1)`,
				escName); err != nil {
				return empty, fmt.Errorf("Getting role: %v", err)
			} else {
				defer rows.Close()
				if rows.Next() {
					userExists = true
				}
			}
			if rows, err := db.Query(`SELECT r.oid::regrole::text AS rolename FROM pg_roles r
                                                  JOIN pg_auth_members m ON m.roleid = oid
                                                  JOIN pg_roles u ON u.oid = m.member
                                                  AND LOWER(u.oid::regrole::text) = LOWER($1)
                                                  ORDER BY rolename`,
				escName); err != nil {
				return empty, err
			} else {
				defer rows.Close()
				toAdd, toDelete := []udb{}, []string{}
				k := 0
			loop:
				for rows.Next() {
					var d string
					if err = rows.Scan(&d); err != nil {
						return empty, fmt.Errorf("Getting role: %v", err)
					}
					if strings.EqualFold(d, escName) {
						userExists = true
						continue
					}
					for k != len(ds) {
						switch {
						case d == ds[k].u:
							k += 1
							continue loop
						case ds[k].u < d:
							toAdd = append(toAdd, ds[k])
							k += 1
						case ds[k].u > d:
							toDelete = append(toDelete, d)
							continue loop
						}
					}
					// we can get here only when k == len(ds)
					toDelete = append(toDelete, d)
				}
				if k < len(ds) {
					toAdd = append(toAdd, ds[k:]...)
				}
				// log.Println("toAdd:", toAdd, "toDelete:", toDelete)
				if !userExists {
					_, err = db.Exec(fmt.Sprintf("CREATE USER %s WITH ENCRYPTED PASSWORD '%s'",
						escName, esc(userCnf.Password)))
					if err != nil {
						return empty, err
					}
					log.Printf("CREATing USER %s", escName)
				} else {
					_, err = db.Exec(fmt.Sprintf("ALTER USER %s WITH ENCRYPTED PASSWORD '%s'",
						escName, esc(userCnf.Password)))
					if err != nil {
						return empty, err
					}
					log.Printf("ALTERing USER %s", escName)
				}
				for _, d := range toDelete {
					_, err = db.Exec(fmt.Sprintf("REVOKE %s FROM %s", d, escName))
					if err != nil {
						return empty, err
					}
					log.Printf("REVOKE %s FROM %s", d, escName)
				}
				for _, a := range toAdd {
					_, err = db.Exec(fmt.Sprintf(`GRANT %s TO %s`, a.u, escName))
					if err != nil {
						return empty, err
					}
					/*
						_, err = db.Exec(fmt.Sprintf(`ALTER ROLE %s IN DATABASE %s SET ROLE TO %s`,
							escName, a.db, a.u))
						if err != nil {
							return empty, err
						}
					*/
					log.Printf(`GRANT %s TO %s`, a.u, escName)
				}
			}
		}
	}

	return empty, nil
}
