package main

import (
	"dymium.com/dymium/log"

	"database/sql"

	_ "github.com/lib/pq"

	"fmt"

	"crypto/sha256"

        . "DbSetup"
	"DbSetup/types"
)

func doUpdate(
	cnf *guardianConf,
	datascope *types.Scope,
	connections *map[string]types.Connection,
	credentials *map[string]types.Credential) (empty struct{}, err error) {

	sslmode_ := "disable"
	if *cnf.GuardianTls {
		sslmode_ = "require"
	}

	connectStr := fmt.Sprintf("host=%%s port=%d dbname='%%s' user=%s password='%s' sslmode=%s",
		cnf.GuardianPort, cnf.GuardianUser, cnf.GuardianAdminPassword, sslmode_)

	localUser := fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(datascope.Name+"_dymium")))

	for _, a := range cnf.GuardianAddress {
		if db, err := sql.Open("postgres", fmt.Sprintf(connectStr, a, esc(cnf.GuardianDatabase))); err != nil {
			log.Errorf("%s connection: Cannot open connection. Ignoring error: %v", a, err)
			continue
		} else {
			defer db.Close()
			if rows, err := db.Query(fmt.Sprintf("SELECT 1 FROM pg_database WHERE datname = '%s'", esc(datascope.Name))); err != nil {
				log.Errorf("%s connection: Can not query database name '%s': %v", a, esc(datascope.Name), err)
				return empty, fmt.Errorf("%s connection: Can not query database name '%s': %v", a, esc(datascope.Name), err)
			} else {
				defer rows.Close()

				if !rows.Next() {
					sql := fmt.Sprintf("CREATE DATABASE %s OWNER %s", LiteralEscape(datascope.Name), cnf.GuardianUser)
					log.Infof(sql)
					if _, err = db.Exec(sql); err != nil {
						return empty, fmt.Errorf("%s connection: Can not create database %s: %v", a, LiteralEscape(datascope.Name), err)
					}
					sql = fmt.Sprintf("REVOKE CONNECT ON DATABASE %s FROM PUBLIC", LiteralEscape(datascope.Name))
					log.Infof(sql)
					if _, err = db.Exec(sql); err != nil {
						return empty, fmt.Errorf("%s connection: Can not revoke access to database %s: %v", a, LiteralEscape(datascope.Name), err)
					}
					sql = fmt.Sprintf("CREATE ROLE %s", localUser)
					log.Infof(sql)
					if _, err = db.Exec(sql); err != nil {
						return empty, fmt.Errorf("%s connection: Can not create role %s: %v", a, localUser, err)
					}
					sql = fmt.Sprintf("GRANT CONNECT ON DATABASE %s TO %s", LiteralEscape(datascope.Name), localUser)
					log.Infof(sql)
					if _, err = db.Exec(sql); err != nil {
						return empty, fmt.Errorf("%s connection: Can not grant connect on %s to %s: %v",
							a, LiteralEscape(datascope.Name), localUser, err)
					}
				}
			}
		}
		if db, err := sql.Open("postgres", fmt.Sprintf(connectStr, a, esc(datascope.Name))); err != nil {
			return empty, fmt.Errorf("%s connection: Cannot open connection to %s: %v", a, LiteralEscape(datascope.Name), err)
		} else {
			defer db.Close()
			if err = clearDatabase(db); err != nil {
				return empty, fmt.Errorf("%s connection: Clearing %s: %v", a, LiteralEscape(datascope.Name), err)
			}
			if err = ConfigureDatabase(db, datascope, *connections, *credentials, false); err != nil {
				return empty, fmt.Errorf("%s connection: Configuring %s: %v", a, LiteralEscape(datascope.Name), err)
			}
		}
	}

	return empty, nil
}

func clearDatabase(db *sql.DB) error {

	exec := func(sql string) error {
		log.Infof(sql)
		if _, err := db.Exec(sql); err != nil {
			return fmt.Errorf("%s failed: %v", err, err)
		}
		return nil
	}

	if rows, err := db.Query("SELECT 1 FROM information_schema.schemata WHERE schema_name = '_dymium'"); err != nil {
		return fmt.Errorf("looking for _dymium failed: %v", err)
	} else {
		defer rows.Close()
		if rows.Next() {
			if rsrv, err := db.Query("SELECT server FROM _dymium.servers"); err != nil {
				return fmt.Errorf("Getting list of servers: %v", err)
			} else {
				defer rsrv.Close()
				for rsrv.Next() {
					var server string
					if err = rsrv.Scan(&server); err != nil {
						return fmt.Errorf("Getting list of servers: %v", err)
					}
					exec(fmt.Sprintf("DROP SERVER %q CASCADE", server))
				}
				if _, err := db.Exec("DELETE FROM _dymium.servers"); err != nil {
					return fmt.Errorf("DELETE FROM _dymium.servers: %v", err)
				}
			}
			if rsch, err := db.Query("SELECT \"schema\" FROM _dymium.schemas"); err != nil {
				return fmt.Errorf("Getting list of schemas: %v", err)
			} else {
				defer rsch.Close()
				for rsch.Next() {
					var schema string
					if err = rsch.Scan(&schema); err != nil {
						return fmt.Errorf("Getting list of schemas: %v", err)
					}
					if schema != "public" {
						exec(fmt.Sprintf("DROP SCHEMA %q CASCADE", schema))
					}
				}
				if _, err := db.Exec("DELETE FROM _dymium.schemas"); err != nil {
					return fmt.Errorf("DELETE FROM _dymium.schemas: %v", err)
				}
			}
		} else {
			if err := exec("CREATE SCHEMA _dymium"); err != nil {
				return err
			}
			if err := exec("CREATE TABLE _dymium.servers ( server text )"); err != nil {
				return err
			}
			if err := exec("CREATE TABLE _dymium.schemas ( \"schema\" text )"); err != nil {
				return err
			}
		}
	}

	return nil
}
