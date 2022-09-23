package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"log"
	"sort"

	"DbSync/types"
)

func confUser(
	userCnf *types.UserConf,
	cnf *guardianConf,
) (empty struct{}, err error) {

	if userCnf == nil {
		return empty, fmt.Errorf("User conf is not defined")
	}

	log.Printf("confUser: { Name: %s, Datascopes: %v }\n",
		userCnf.Name, userCnf.Datascopes)
	log.Printf("guardianCnf: { Address: %v:%d, Database: %s}\n",
		cnf.GuardianAddress, cnf.GuardianPort, cnf.GuardianDatabase)

	sslmode_ := "disable"
	if *cnf.GuardianTls {
		sslmode_ = "require"
	}

	ds := make([]string, len(userCnf.Datascopes))
	copy(ds, userCnf.Datascopes)
	sort.Strings(ds)

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
                                                  join  pg_auth_members m on m.roleid = oid
                                                  join pg_roles u on u.oid = m.member
                                                  and u.oid::regrole::text = $1
                                                  ORDER BY rolename`,
				userCnf.Name); err != nil {
				return empty, err
			} else {
				defer rows.Close()
				toAdd, toDelete := []string{}, []string{}
				k := 0
				for rows.Next() {
					var d string
					if err = rows.Scan(&d); err != nil {
						return empty, fmt.Errorf("Getting role: %v", err)
					}
					if k == len(ds) {
						switch d {
						case userCnf.Name:
							userExists = true
						default:
							toDelete = append(toDelete, d)
						}
					} else {
						switch d {
						case userCnf.Name:
							userExists = true
						case ds[k]:
							k += 1
						default:
							if d < ds[k] {
								log.Printf("toDelete: d=%s ds[%d]=%s\n", d, k, ds[k])
								toDelete = append(toDelete, d)
							} else {
								log.Printf("toAdd: d=%s ds[%d]=%s\n", d, k, ds[k])
								toAdd = append(toAdd, d)
								k += 1
							}
						}
					}
				}
				if k < len(ds) {
					toAdd = append(toAdd, ds[k:]...)
				}
				log.Println("toAdd:", toAdd, "toDelete:", toDelete)
				if !userExists {
					_, err = db.Exec(fmt.Sprintf("CREATE USER %s WITH ENCRYPTED PASSWORD '%s'", userCnf.Name, userCnf.Password))
					if err != nil {
						return empty, err
					}
					log.Printf("CREATing USER %s", userCnf.Name)
				} else {
					_, err = db.Exec(fmt.Sprintf("ALTER USER %s WITH ENCRYPTED PASSWORD '%s'", userCnf.Name, userCnf.Password))
					if err != nil {
						return empty, err
					}
					log.Printf("ALTERing USER %s", userCnf.Name)
				}
				for _, d := range toDelete {
					_, err = db.Exec(fmt.Sprintf("REVOKE %s FROM %s", d, userCnf.Name))
					if err != nil {
						return empty, err
					}
					log.Printf("REVOKE %s FROM %s", d, userCnf.Name)
				}
				for _, a := range toAdd {
					_, err = db.Exec(fmt.Sprintf("GRANT %s TO %s", a, userCnf.Name))
					if err != nil {
						return empty, err
					}
					_, err = db.Exec(fmt.Sprintf("ALTER ROLE %s IN DATABASE %s SET ROLE TO %s", userCnf.Name, a, a))
					if err != nil {
						return empty, err
					}
					log.Printf("GRANT %s TO %s", a, userCnf.Name)
				}
			}
		}
	}

	return empty, nil
}
