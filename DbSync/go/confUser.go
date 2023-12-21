package main

import (
	"database/sql"

	_ "github.com/lib/pq"

	"fmt"
	"log"
	"sort"

	"crypto/sha256"

	"DbSync/types"
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
			if rows, err := db.Query(`select true from pg_roles where oid::regrole::text = $1`,
				userCnf.Name); err != nil {
				return empty, fmt.Errorf("Getting role: %v", err)
			} else {
				defer rows.Close()
				if rows.Next() {
					userExists = true
				}
			}
			if rows, err := db.Query(`select r.oid::regrole::text as rolename from pg_roles r
                                                  join pg_auth_members m on m.roleid = oid
                                                  join pg_roles u on u.oid = m.member
                                                  and u.oid::regrole::text = $1
                                                  ORDER BY rolename`,
				userCnf.Name); err != nil {
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
					if d == userCnf.Name {
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
						PostgresEscape(userCnf.Name), esc(userCnf.Password)))
					if err != nil {
						return empty, err
					}
					log.Printf("CREATing USER %s", PostgresEscape(userCnf.Name))
				} else {
					_, err = db.Exec(fmt.Sprintf("ALTER USER %s WITH ENCRYPTED PASSWORD '%s'",
						PostgresEscape(userCnf.Name), esc(userCnf.Password)))
					if err != nil {
						return empty, err
					}
					log.Printf("ALTERing USER %s", PostgresEscape(userCnf.Name))
				}
				for _, d := range toDelete {
					_, err = db.Exec(fmt.Sprintf("REVOKE %s FROM %s", d, PostgresEscape(userCnf.Name)))
					if err != nil {
						return empty, err
					}
					log.Printf("REVOKE %s FROM %s", d, PostgresEscape(userCnf.Name))
				}
				for _, a := range toAdd {
					_, err = db.Exec(fmt.Sprintf(`GRANT %s TO %s`, a.u, PostgresEscape(userCnf.Name)))
					if err != nil {
						return empty, err
					}
					/*
						_, err = db.Exec(fmt.Sprintf(`ALTER ROLE %s IN DATABASE %s SET ROLE TO %s`,
							PostgresEscape(userCnf.Name), a.db, a.u))
						if err != nil {
							return empty, err
						}
					*/
					log.Printf(`GRANT %s TO %s`, a.u, PostgresEscape(userCnf.Name))
				}
			}
		}
	}

	return empty, nil
}
