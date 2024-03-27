package DbSetup

import (
	"dymium.com/dymium/log"

	"context"
	"fmt"
	"strings"

	"database/sql"

	"crypto/sha256"

	"dymium.io/DbSetup/types"
)

type ct_option struct {
	ext        string
	server_def func(server, address string, port int, database string, use_tls bool, user string, password string) (string, error)
	table      func(remoteSchema, remoteTable string) string
}

var ct_options map[types.ConnectionType]ct_option

var obf func(name string) (string, string)

var redact_value func(t string) string

func ConfigureDatabase(db *sql.DB,
	datascope *types.Scope,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
	createDymiumTables bool) error {

	log.Infof("configureDatabase: datascope=%+v\n", datascope)
	log.Infof("configureDatabase: connections=%+v\n", connections)

	localUser := fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(datascope.Name+"_dymium")))

	type tuple struct {
		k1 string
		k2 string
	}
	shortEntries := make(map[tuple]int)
	longSchemas := map[tuple]struct{}{}
	for k := range datascope.Schemas {
		s := &datascope.Schemas[k]
		for m := range s.Tables {
			t := s.Tables[m]
			se := tuple{k1: PostgresEscape(s.Name), k2: PostgresEscape(t.Name)}
			shortEntries[se]++
			c := connections[t.Connection].Name
			longSchemas[tuple{k1: c, k2: s.Name}] = struct{}{}
		}
	}
	{
		toDel := make([]tuple, 0, len(longSchemas))
		for se, n := range shortEntries {
			if n > 0 {
				toDel = append(toDel, se)
			}
		}
		for _, se := range toDel {
			delete(shortEntries, se)
		}
	}
	shortSchemas := map[string]struct{}{}
	for se := range shortEntries {
		shortSchemas[se.k1] = struct{}{}
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

	exec := func(sql string, args ...interface{}) error {
		log.Infof(sql)
		if _, err := tx.ExecContext(ctx, sql, args...); err != nil {
			return rollback(err, "["+sql+"] failed")
		}
		return nil
	}

	// This should go first: this schema is used by other initializers!
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

	if err = setupVault(exec, localUser); err != nil {
		return err
	}
	if err = setupObfuscator(exec, localUser); err != nil {
		return err
	}
	if err = setupConnections(exec, datascope.Connections, connections, credentials); err != nil {
		return err
	}

	for k := range shortSchemas {
		// k is already PostgresEscaped!
		if err = exec("CREATE SCHEMA IF NOT EXISTS " + k); err != nil {
			return err
		}
		if err = exec("GRANT USAGE ON SCHEMA " + k + " TO " + localUser); err != nil {
			return err
		}
		if err = exec("ALTER DEFAULT PRIVILEGES IN SCHEMA " + k + " GRANT SELECT ON TABLES TO " + localUser); err != nil {
			return err
		}
		if err = exec("INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", k); err != nil {
			return err
		}
	}
	for k := range longSchemas {
		kk := PostgresEscape(k.k1 + "_" + k.k2)
		if err := exec(fmt.Sprintf("CREATE SCHEMA IF NOT EXISTS %s", kk)); err != nil {
			return err
		}
		if err := exec(fmt.Sprintf("GRANT USAGE ON SCHEMA %s TO %s", kk, localUser)); err != nil {
			return err
		}
		if err := exec(fmt.Sprintf("ALTER DEFAULT PRIVILEGES IN SCHEMA %s GRANT SELECT ON TABLES TO %s", kk, localUser)); err != nil {
			return err
		}
		if err := exec("INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", kk); err != nil {
			return err
		}
	}

	for k := range datascope.Schemas {
		s := &datascope.Schemas[k]
		for m := range s.Tables {
			t := &s.Tables[m]
			con := connections[t.Connection]
			hiddenTblName := fmt.Sprintf("_dymium._%x_", sha256.Sum224([]byte(datascope.Name+s.Name+t.Name)))
			hiddenTblCols, viewDef := make([]string, 0, len(t.Columns)), make([]string, 0, len(t.Columns))
			isDuplicate := findDuplicates(t.Columns, func(c types.Column) string { return c.Name })
			for k := range t.Columns {
				pesc := PostgresEscape
				if isDuplicate[k] {
					pesc = LiteralEscape
				}
				c := &t.Columns[k]
				{
					t := c.Typ
					if c.Semantics == "UNSUPPORTED" {
						t = "bytea"
					}

					v := LiteralEscape(c.Name) + ` ` + t
					if !c.IsNullable {
						v += " NOT NULL"
					}
					hiddenTblCols = append(hiddenTblCols, "  "+v)
				}
				switch c.Action {
				case types.DH_Block:
				case types.DH_Redact:
					if c.IsNullable {
						viewDef = append(viewDef, "CAST(NULL AS "+c.Typ+") AS "+pesc(c.Name))
					} else if strings.HasSuffix(c.Typ, "[]") {
						viewDef = append(viewDef, "CAST('{}' AS "+c.Typ+") AS "+pesc(c.Name))
					} else {
						viewDef = append(viewDef, "CAST("+redact_value(c.Typ)+" AS "+c.Typ+") AS "+pesc(c.Name))
					}
				case types.DH_Obfuscate:
					var v string
					if strings.HasSuffix(c.Typ, "[]") {
						switch {
						case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
							n, k := obf("obfuscate_text_array")
							v = fmt.Sprintf(`_dymium.%s(%s,%s,0,false) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
							n, k := obf("obfuscate_text_array")
							v = fmt.Sprintf(`_dymium.%s(%s,%s,0,true) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						case strings.HasPrefix(c.Typ, "uuid"):
							n, k := obf("obfuscate_uuid_array")
							v = fmt.Sprintf(`_dymium.%s(%s,%s) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						default:
							panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
						}
					} else {
						switch {
						case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
							n, k := obf("obfuscate_text")
							v = fmt.Sprintf(`_dymium.%s(%s,%s,0,false) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
							n, k := obf("obfuscate_text")
							v = fmt.Sprintf(`_dymium.%s(%s,%s,0,true) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						case strings.HasPrefix(c.Typ, "uuid"):
							n, k := obf("obfuscate_uuid")
							v = fmt.Sprintf(`_dymium.%s(%s,%s) AS %s`,
								n, k, LiteralEscape(c.Name), pesc(c.Name))
						default:
							panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
						}
					}
					viewDef = append(viewDef, v)
				case types.DH_Allow:
					viewDef = append(viewDef, LiteralEscape(c.Name)+` AS `+pesc(c.Name))
				}
			}
			//con := connections[t.Connection]
			var schemaname string
			if con.Database_type == types.CT_MongoDB {
				// in case of MongoDB, schemaname is dbname
				schemaname = con.Dbname
			} else {
				schemaname = s.Name
			}
			opts := ct_options[con.Database_type]

			hiddenTbl := "CREATE FOREIGN TABLE " + hiddenTblName + " (\n" +
				strings.Join(hiddenTblCols, ",\n") +
				"\n) SERVER " + serverName(con.Name) + " OPTIONS(" + opts.table(schemaname, t.Name) + ");\n"
			view :=
				fmt.Sprintf("CREATE VIEW %%s.%s AS SELECT %s FROM %s;\n",
					PostgresEscape(t.Name),
					strings.Join(viewDef, ", "),
					hiddenTblName)

			if err := exec(hiddenTbl); err != nil {
				return err
			}

			schs := []string{con.Name + "_" + s.Name}
			if _, ok := shortEntries[tuple{k1: PostgresEscape(s.Name), k2: PostgresEscape(t.Name)}]; ok {
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

func serverName(str string) string {
	return str + "_server"
}
