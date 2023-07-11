package main

import (
	"context"
	"fmt"
	"log"
	"strings"

	"database/sql"

	"crypto/sha256"

	"DbSync/types"
)

var obf_funcs func() []string
var obf func(name string) (string, string)

func init() {
	on := func(n string) string {
		return fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(n)))
	}
	type oT struct {
		n string
		k int
	}
	lst := []oT{
		{
			n: "obfuscate_text",
			k: 0x226193b1,
		},
		{
			n: "obfuscate_text_array",
			k: 0xf8bfced0,
		},
		{
			n: "obfuscate_uuid",
			k: 0x4b0a02b,
		},
		{
			n: "obfuscate_uuid_array",
			k: 0xced64e86,
		},
	}
	type obfT struct {
		n string
		k string
	}
	obfS := make(map[string]obfT, len(lst))
	for _, o := range lst {
		obfS[o.n] = obfT{
			n: on(o.n),
			k: fmt.Sprintf("x'%x'::int", o.k),
		}
	}
	obf = func(name string) (string, string) {
		o, ok := obfS[name]
		if !ok {
			panic(fmt.Sprintf("function [obf] is called with wrong argument [%s]", name))
		}
		return o.n, o.k
	}
	obf_funcs = func() []string {
		r := make([]string, 0, len(lst))
		for _, o := range lst {
			r = append(r, obfS[o.n].n)
		}
		return r
	}
}

func extensionName(connectionType types.ConnectionType) (string, error) {
	switch connectionType {
	case types.CT_PostgreSQL:
		return "postgres_fdw", nil
	case types.CT_MySQL, types.CT_MariaDB:
		return "mysql_fdw", nil
	case types.CT_SqlServer:
		return "tds_fdw", nil
	case types.CT_OracleDB:
		return "oracle_fdw", nil
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
	case types.CT_MySQL, types.CT_MariaDB:
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
	case types.CT_SqlServer:
		return iOptions{
			server: func(host string, port int, dbname string) string {
				return fmt.Sprintf("servername '%s', port '%d', database '%s'",
					esc(host), port, esc(dbname))
			},
			userMapping: func(user, password string) string {
				return fmt.Sprintf("username '%s', password '%s'",
					esc(user), esc(password))
			},
			table: func(remoteSchema, remoteTable string) string {
				return fmt.Sprintf("schema_name '%s', table_name '%s'",
					esc(remoteSchema), esc(remoteTable))
			},
		}
	case types.CT_OracleDB:
		return iOptions{
			server: func(host string, port int, dbname string) string {
				return fmt.Sprintf("dbserver '//%s:%d/%s'",
					host, port, strings.ToUpper(dbname))
			},
			userMapping: func(user, password string) string {
				return fmt.Sprintf("user '%s', password '%s'",
					esc(user), esc(password))
			},
			table: func(remoteSchema, remoteTable string) string {
				return fmt.Sprintf("schema '%s', table '%s'",
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

	// log.Printf("configureDatabase: datascope=%+v connections=%+v\n",datascope,connections)

	localUser := fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(datascope.Name+"_dymium")))

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

	if err = exec("CREATE EXTENSION IF NOT EXISTS obfuscator WITH SCHEMA _dymium"); err != nil {
		return err
	}
	for _, f := range obf_funcs() {
		if err := exec("GRANT EXECUTE ON FUNCTION _dymium." + f + " TO " + localUser); err != nil {
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
                                      CREATE USER MAPPING FOR public
                                      SERVER `+c.Name+`_server
                                      OPTIONS (%s)`,
				opts.userMapping(cred.User_name, cred.Password))
			if _, err := tx.ExecContext(ctx, sql); err != nil {
				errSql := fmt.Sprintf(`
                                      CREATE USER MAPPING FOR public
                                      SERVER `+c.Name+`_server
                                      OPTIONS (%s)`,
					opts.userMapping(cred.User_name, "******"))
				return rollback(err, "["+errSql+"] failed")
			}
		}
	}

	for k := range shortSchemas {
		if err := exec("CREATE SCHEMA IF NOT EXISTS " + strings.ToLower(k)); err != nil {
			return err
		}
		if err := exec("GRANT USAGE ON SCHEMA " + strings.ToLower(k) + " TO " + localUser); err != nil {
			return err
		}
		if err := exec("ALTER DEFAULT PRIVILEGES IN SCHEMA " + strings.ToLower(k) + " GRANT SELECT ON TABLES TO " + localUser); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", strings.ToLower(k)); err != nil {
			return rollback(err, "Registering schema "+strings.ToLower(k)+"_server failed")
		}
	}
	for k := range longSchemas {
		kk := strings.ToLower(k.k1 + "_" + k.k2)
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
			hiddenTblName := fmt.Sprintf("_dymium._%x_", sha256.Sum224([]byte(datascope.Name+s.Name+t.Name)))
			// hiddenViewName := fmt.Sprintf("_dymium._%x_", sha256.Sum224([]byte(datascope.Name+s.Name+t.Name+"_VIEW")))
			hiddenTblCols, viewDef := make([]string, 0, len(t.Columns)), make([]string, 0, len(t.Columns))
			for k := range t.Columns {
				c := &t.Columns[k]
				switch c.Action {
				case types.DH_Block:
					continue
				case types.DH_Redact:
					var v string
					if c.IsNullable {
						v = "NULL"
					} else if strings.HasSuffix(c.Typ, "[]") {
						v = "'{}'"
					} else {
						switch {
						case
							c.Typ == "interval":
							v = "'0 days'"
						case
							strings.HasPrefix(c.Typ, "char"),
							strings.HasPrefix(c.Typ, "var"),
							c.Typ == "text",
							c.Typ == "bpchar":
							v = "'{}'"
						case
							c.Typ == "bigint",
							strings.HasPrefix(c.Typ, "double"),
							strings.HasPrefix(c.Typ, "int"),
							strings.HasPrefix(c.Typ, "numeric"),
							c.Typ == "real",
							c.Typ == "smallint",
							strings.HasPrefix(c.Typ, "float"),
							strings.HasPrefix(c.Typ, "decimal"):
							v = "0"
						case
							strings.HasPrefix(c.Typ, "bool"):
							v = "false"
						case
							c.Typ == "xml":
							v = "''"
						case
							c.Typ == "bytea":
							v = "E'\\x'"
						case
							c.Typ == "json", c.Typ == "jsonb":
							v = "'{}'"
						case
							c.Typ == "uuid":
							v = "'00000000-0000-0000-0000-000000000000'"
						case
							strings.HasPrefix(c.Typ, "timestamp"):
							if strings.HasSuffix(c.Typ, "with timezone") || strings.HasSuffix(c.Typ, "with time zone") {
								v = "'2000-01-01 00:00:00 UTC'"
							} else {
								v = "'0001-01-01 00:00:00'"
							}
						case
							strings.HasPrefix(c.Typ, "time"):
							if strings.HasSuffix(c.Typ, "with timezone") || strings.HasSuffix(c.Typ, "with time zone") {
								v = "'00:00:00 UTC'"
							} else {
								v = "'00:00:00'"
							}
						case
							c.Typ == "date":
							v = "'0001-01-01'"
						}
						viewDef = append(viewDef, "CAST("+v+" AS "+c.Typ+") AS "+PostgresEscape(c.Name))
					}
				case types.DH_Obfuscate:
					v := PostgresEscape(c.Name) + " " + c.Typ + " OPTIONS( redact '0' )"
					if !c.IsNullable {
						v += " NOT NULL"
					}
					hiddenTblCols = append(hiddenTblCols, "  "+v)
					// viewDef = append(viewDef,"    obfuscate("+PostgresEscape(c.Name)+")")
					if strings.HasSuffix(c.Typ, "[]") {
						switch {
						case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
							n, k := obf("obfuscate_text_array")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,false) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
							n, k := obf("obfuscate_text_array")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,true) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						case strings.HasPrefix(c.Typ, "uuid"):
							n, k := obf("obfuscate_uuid_array")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,true) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						default:
							panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
						}
					} else {
						switch {
						case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
							n, k := obf("obfuscate_text")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,false) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
							n, k := obf("obfuscate_text")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,true) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						case strings.HasPrefix(c.Typ, "uuid"):
							n, k := obf("obfuscate_uuid")
							v = fmt.Sprintf("_dymium.%s(%s,%s,0,true) AS %s",
								n, k, PostgresEscape(c.Name), PostgresEscape(c.Name))
						default:
							panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
						}
					}
					viewDef = append(viewDef, v)
				case types.DH_Allow:
					v := PostgresEscape(c.Name) + " " + c.Typ + " OPTIONS( redact '0' )"
					if !c.IsNullable {
						v += " NOT NULL"
					}
					hiddenTblCols = append(hiddenTblCols, "  "+v)
					viewDef = append(viewDef, PostgresEscape(c.Name))
				}
			}
			con := connections[t.Connection]
			opts := options(con.Database_type)
			hiddenTbl := "CREATE FOREIGN TABLE " + hiddenTblName + " (\n" +
				strings.Join(hiddenTblCols, ",\n") +
				"\n) SERVER " + con.Name + "_server OPTIONS(" + opts.table(s.Name, t.Name) + ");\n"
			view :=
				fmt.Sprintf("CREATE VIEW %%s.%s AS SELECT %s FROM %s;\n",
					PostgresEscape(t.Name),
					strings.Join(viewDef, ", "),
					hiddenTblName)

			if err := exec(hiddenTbl); err != nil {
				return err
			}
			// if err := exec(hiddenView); err != nil {
			//	return err
			// }

			schs := []string{con.Name + "_" + s.Name}
			if _, ok := shortEntries[tuple{k1: s.Name, k2: t.Name}]; ok {
				if s.Name == "public" {
					schs = append(schs, "public", con.Name)
				} else {
					schs = append(schs, s.Name)
				}
			}
			for _, sch := range schs {
				if err := exec(fmt.Sprintf(view, PostgresEscape(sch))); err != nil {
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
