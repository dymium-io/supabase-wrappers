package main

import (
	"context"
	"fmt"
	"log"
	"strings"

	"database/sql"

	"initializer/types"
)

func extensionName(connectionType types.ConnectionType) (string, error) {
	switch connectionType {
	case types.CT_PostgreSQL:
		return "postgres_fdw", nil
	case types.CT_MySQL:
		return "mysql_fdw", nil
	}
	return "", fmt.Errorf("Extension %v is not supported yet", connectionType)
}

type iOptions struct {
	server      func(host string, port int, dbname string) string
	userMapping func(user, password string) string
	table       func(remoteSchema, remoteTable string) string
}

func options(connectionType types.ConnectionType) iOptions {
	switch connectionType {
	case types.CT_PostgreSQL:
		return iOptions{
			server: func(host string, port int, dbname string) string {
				return fmt.Sprintf("host '%s', port '%d', dbname '%s'",
					esc(host), port, esc(dbname))
			},
			userMapping: func(user, password string) string {
				return fmt.Sprintf("user '%s', password '%s'",
					esc(user), esc(password))
			},
			table: func(remoteSchema, remoteTable string) string {
				return fmt.Sprintf("schema_name '%s', table_name '%s'",
					esc(remoteSchema), esc(remoteTable))
			},
		}
	case types.CT_MySQL:
		return iOptions{
			server: func(host string, port int, dbname string) string {
				return fmt.Sprintf("host '%s', port '%d'",
					esc(host), port)
			},
			userMapping: func(user, password string) string {
				return fmt.Sprintf("username '%s', password '%s'",
					esc(user), esc(password))
			},
			table: func(remoteSchema, remoteTable string) string {
				return fmt.Sprintf("dbname '%s', table_name '%s'",
					esc(remoteSchema), esc(remoteTable))
			},
		}
	}
	panic("impossible")
}

func configureDatabase(db *sql.DB,
	datascope *types.Scope,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
	createDymiumTables bool) error {

	localUser := datascope.Name

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
		shortSchemas[s.Name] = struct{}{}
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
	}

	for k := range datascope.Connections {
		c, ok := connections[datascope.Connections[k]]
		if !ok {
			return rollback(fmt.Errorf("misconfiguration!"),
				fmt.Sprintf("Connection %s is not defined", datascope.Connections[k]))
		}
		e, _ := extensionName(c.Database_type)

		opts := options(c.Database_type)

		sql := `CREATE SERVER ` + c.Name + `_server FOREIGN DATA WRAPPER ` + e + ` OPTIONS (` +
			opts.server(c.Address, c.Port, c.Dbname) + `)`
		if err := exec(sql); err != nil {
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

		{
			// Don't use exec(), because sql contains password
			sql := fmt.Sprintf(`
                                      CREATE USER MAPPING FOR `+localUser+`
                                      SERVER `+c.Name+`_server
                                      OPTIONS (%s)`,
				opts.userMapping(cred.User_name, cred.Password))
			if _, err := tx.ExecContext(ctx, sql); err != nil {
				return rollback(err, "["+sql+"] failed")
			}
		}
	}

	for k := range shortSchemas {
		if err := exec("CREATE SCHEMA IF NOT EXISTS " + k); err != nil {
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
		if err := exec(fmt.Sprintf("CREATE SCHEMA IF NOT EXISTS %q", kk)); err != nil {
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
					ract := 0
					rtyp := 0
					rnul := 0
					notNull := " NOT NULL"
					if c.IsNullable {
						notNull = ""
						rnul = 1
					}
					switch c.Action {
					case "Redact":
						ract = 0x1
					case "Obfuscate":
						ract = 0x2
					case "Smart Redact":
						ract = 0x0
					}
					switch {
					case
						strings.HasPrefix(c.Typ, "char"),
						strings.HasPrefix(c.Typ, "var"),
						c.Typ == "text",
						c.Typ == "bpchar":
						rtyp = 0x1
					case
						c.Typ == "bigint",
						strings.HasPrefix(c.Typ, "double"),
						strings.HasPrefix(c.Typ, "int"),
						strings.HasPrefix(c.Typ, "numeric"),
						c.Typ == "real",
						c.Typ == "smallint",
						strings.HasPrefix(c.Typ, "float"),
						strings.HasPrefix(c.Typ, "decimal"):
						rtyp = 0x2
					case
						strings.HasPrefix(c.Typ, "bool"):
						rtyp = 0x3
					case
						c.Typ == "xml":
						rtyp = 0x4
					case
						c.Typ == "bytea":
						rtyp = 0x5
					case
						c.Typ == "json", c.Typ == "jsonb":
						rtyp = 0x6
					case
						c.Typ == "uuid":
						rtyp = 0x7
					}
					defs = append(defs, fmt.Sprintf("  %q %s OPTIONS( redact '%d' )%s",
						c.Name, c.Typ, ract|(rnul<<2)|(rtyp<<3), notNull))
				}
			}
			e := "CREATE FOREIGN TABLE %q.%q (\n" + strings.Join(defs, ",\n") + "\n)\n" +
				"  SERVER %s_server OPTIONS(%s)"

			con := connections[t.Connection]
			opts := options(con.Database_type)
			schs := []string{con.Name + "_" + s.Name}
			if _, ok := shortEntries[tuple{k1: s.Name, k2: t.Name}]; ok {
				if s.Name == "public" {
					schs = append(schs, "public", con.Name)
				} else {
					schs = append(schs, s.Name)
				}
			}
			for _, sch := range schs {
				if err := exec(fmt.Sprintf(e, sch, t.Name, con.Name, opts.table(s.Name, t.Name))); err != nil {
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
