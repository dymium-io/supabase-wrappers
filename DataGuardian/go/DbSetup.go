package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"

	"database/sql"

	"initializer/types"
)

func extensionName(connectionType types.ConnectionType) (string, error) {
	switch connectionType {
	case types.CT_PostgreSQL:
		return "postgres_fdw", nil
	}
	return "", fmt.Errorf("Extension %v is not supported yet", connectionType)
}

func configureDatabase(db *sql.DB,
	datascope *types.Datascope,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
	createDymiumTables bool) error {

	localUser := os.Getenv("TEST_USER")
	if localUser == "" {
		return fmt.Errorf("Environment variable TEST_USER is not defined")
	}

	connectionTypes := map[types.ConnectionType]struct{}{}
	for k := range datascope.Connections {
		connectionTypes[connections[datascope.Connections[k]].Database_type] = struct{}{}
	}

	type tuple struct {
		k1 string
		k2 string
	}
	shortEntries := map[tuple]struct{}{}
	longSchemas := map[tuple]struct{}{}
	shortSchemas := map[string]struct{}{}
	for k := range datascope.Schemas {
		s := &datascope.Schemas[k]
		if s.Name != "public" {
			shortSchemas[s.Name] = struct{}{}
		}
		for m := range s.Tables {
			t := s.Tables[m]
			se := tuple{k1: s.Name, k2: t.Name}
			if m > 0 && t.Name == s.Tables[m-1].Name {
				delete(shortEntries, se)
			} else {
				shortEntries[se] = struct{}{}
			}
			c := connections[t.Connection].Name
			longSchemas[tuple{k1: c, k2: s.Name}] = struct{}{}
			if s.Name == "public" {
				shortSchemas[c] = struct{}{}
			}
		}
	}

	ctx := context.Background()
	tx, err := db.BeginTx(ctx, &sql.TxOptions{Isolation: sql.LevelSerializable})
	if err != nil {
		return err
	}

	rollback := func(err error, msg string) error {
		if rollbackErr := tx.Rollback(); rollbackErr != nil {
			return fmt.Errorf("%s: %v, unable to rollback: %v\n", msg, err, rollbackErr)
		} else {
			return fmt.Errorf("%s: %v", msg, err)
		}
	}

	exec := func(sql string) error {
		log.Println(sql)
		if _, err := tx.ExecContext(ctx, sql); err != nil {
			return rollback(err, "["+sql+"] failed")
		}
		return nil
	}

	for ct := range connectionTypes {
		if e, err := extensionName(ct); err != nil {
			return rollback(err, "Configuring extention failed")
		} else if err = exec("CREATE EXTENSION IF NOT EXISTS " + e + " WITH SCHEMA public"); err != nil {
			return err
		}
	}

	if createDymiumTables {
		if err := exec("CREATE SCHEMA _dymium"); err != nil {
			return err
		}
		if err := exec("CREATE TABLE _dymium.servers ( server text )"); err != nil {
			return err
		}
		if err := exec("CREATE TABLE _dymium.schemas ( \"schema\" text )"); err != nil {
			return err
		}

		if err := exec("GRANT USAGE ON SCHEMA public TO " + localUser); err != nil {
			return err
		}
		if err := exec("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO " + localUser); err != nil {
			return err
		}
	}

	for k := range datascope.Connections {
		c, ok := connections[datascope.Connections[k]]
		if !ok {
			return rollback(fmt.Errorf("misconfiguration!"),
				fmt.Sprintf("Connection %s is not defined", datascope.Connections[k]))
		}
		e, _ := extensionName(c.Database_type)

		if err := exec(fmt.Sprintf(`
                                      CREATE SERVER `+c.Name+`_server
                                      FOREIGN DATA WRAPPER `+e+`
                                      OPTIONS (host '%s', port '%d', dbname '%s')`,
			esc(c.Address), c.Port, esc(c.Dbname))); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.servers (server) VALUES ( $1 )", c.Name+"_server"); err != nil {
			return rollback(err, "Registering server "+c.Name+"_server failed")
		}
		cred, ok := credentials[c.Id]
		if !ok {
			return rollback(fmt.Errorf("misconfiguration!"),
				fmt.Sprintf("Can not find credentials for "+c.Id))
		}
		if err := exec(fmt.Sprintf(`
                                      CREATE USER MAPPING FOR `+localUser+`
                                      SERVER `+c.Name+`_server
                                      OPTIONS (user '%s', password '%s')`,
			esc(cred.User_name), esc(cred.Password))); err != nil {
			return err
		}
	}

	for k := range shortSchemas {
		if err := exec("CREATE SCHEMA " + k); err != nil {
			return err
		}
		if err := exec("GRANT USAGE ON SCHEMA " + k + " TO " + localUser); err != nil {
			return err
		}
		if err := exec("ALTER DEFAULT PRIVILEGES IN SCHEMA " + k + " GRANT SELECT ON TABLES TO " + localUser); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", k); err != nil {
			return rollback(err, "Registering schema "+k+"_server failed")
		}
	}
	for k := range longSchemas {
		kk := k.k1 + "_" + k.k2
		if err := exec(fmt.Sprintf("CREATE SCHEMA %q", kk)); err != nil {
			return err
		}
		if err := exec(fmt.Sprintf("GRANT USAGE ON SCHEMA %q TO %s", kk, localUser)); err != nil {
			return err
		}
		if err := exec(fmt.Sprintf("ALTER DEFAULT PRIVILEGES IN SCHEMA %q GRANT SELECT ON TABLES TO %s", kk, localUser)); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", kk); err != nil {
			return rollback(err, "Registering schema "+kk+"_server failed")
		}
	}

	for k := range datascope.Schemas {
		s := &datascope.Schemas[k]
		for m := range s.Tables {
			t := &s.Tables[m]

			defs := []string{}
			for k := range t.Columns {
				c := &t.Columns[k]
				if c.Action != "Block" {
					notNull := " NOT NULL"
					if c.IsNullable {
						notNull = ""
					}
					redact := '0'
					switch c.Action {
					case "Redact": 
						if strings.HasPrefix(c.Typ, "char") ||
							strings.HasPrefix(c.Typ, "var") ||
							c.Typ == "text" ||
							c.Typ == "bpchar" {
							redact = '2'
						} else {
							redact = '1'
						}
					case "Obfuscate": redact = '3'	
					}
					defs = append(defs, fmt.Sprintf("  %q %s OPTIONS( redact '%c' )%s",
						c.Name, c.Typ, redact, notNull))
				}
			}
			e := "CREATE FOREIGN TABLE %q.%q (\n" + strings.Join(defs, ",\n") + "\n)\n" +
				"  SERVER %s_server OPTIONS(schema_name '%s', table_name '%s')"

			con := connections[t.Connection]
			schs := []string{con.Name + "_" + s.Name}
			if _, ok := shortEntries[tuple{k1: s.Name, k2: t.Name}]; ok {
				if s.Name == "public" {
					schs = append(schs, "public", con.Name)
				} else {
					schs = append(schs, s.Name)
				}
			}
			for _, sch := range schs {
				if err := exec(fmt.Sprintf(e, sch, t.Name, con.Name, esc(s.Name), esc(t.Name))); err != nil {
					return err
				}
			}
		}
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("Configure database commit error: %v", err)
	}

	return nil
}

func esc(str string) string {
	var buf strings.Builder
	for _, char := range str {
		switch char {
		case '\\':
			buf.WriteRune('\\')
		case '\'':
			buf.WriteRune('\\')
		}
		buf.WriteRune(char)
	}
	return buf.String()
}
