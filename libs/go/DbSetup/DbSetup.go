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

	/*
		-- temporary disable supabase-related stuff --
		if err = setupVault(exec, localUser); err != nil {
			return err
		}
	*/
	if err = setupObfuscator(exec, localUser); err != nil {
		return err
	}
	if err = setupConnections(exec, datascope.Connections, connections, credentials); err != nil {
		return err
	}

	tv := createTableViews(*datascope, connections)

	for _, s := range tv.schemas {
		if err = exec("CREATE SCHEMA IF NOT EXISTS " + s); err != nil {
			return err
		}
		if err = exec("GRANT USAGE ON SCHEMA " + s + " TO " + localUser); err != nil {
			return err
		}
		if err = exec("ALTER DEFAULT PRIVILEGES IN SCHEMA " + s + " GRANT SELECT ON TABLES TO " + localUser); err != nil {
			return err
		}
		if err = exec("INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", s); err != nil {
			return err
		}
	}

	for k := range tv.tables {
		t := &tv.tables[k]
		con := connections[t.connection]
		hiddenTblCols, viewDef := make([]string, 0, len(t.columns)), make([]string, 0, len(t.columns))
		for k := range t.columns {
			c := &t.columns[k]
			hName := c.Name
			if hName[0] != '"' {
				hName = LiteralEscape(hName)
			}
			{
				t := c.Typ
				if c.Semantics == "UNSUPPORTED" {
					t = "bytea"
				}

				v := hName + " " + t
				if !c.IsNullable {
					v += " NOT NULL"
				}
				hiddenTblCols = append(hiddenTblCols, "  "+v)
			}
			switch c.Action {
			case types.DH_Block:
			case types.DH_Redact:
				if c.IsNullable {
					viewDef = append(viewDef, "CAST(NULL AS "+c.Typ+") AS "+c.Name)
				} else if strings.HasSuffix(c.Typ, "[]") {
					viewDef = append(viewDef, "CAST('{}' AS "+c.Typ+") AS "+c.Name)
				} else {
					viewDef = append(viewDef, "CAST("+redact_value(c.Typ)+" AS "+c.Typ+") AS "+c.Name)
				}
			case types.DH_Obfuscate:
				var v string
				if strings.HasSuffix(c.Typ, "[]") {
					switch {
					case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
						n, k := obf("obfuscate_text_array")
						v = fmt.Sprintf(`_dymium.%s(%s,%s,0,false) AS %s`,
							n, k, hName, c.Name)
					case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
						n, k := obf("obfuscate_text_array")
						v = fmt.Sprintf(`_dymium.%s(%s,%s,0,true) AS %s`,
							n, k, hName, c.Name)
					case strings.HasPrefix(c.Typ, "uuid"):
						n, k := obf("obfuscate_uuid_array")
						v = fmt.Sprintf(`_dymium.%s(%s,%s) AS %s`,
							n, k, hName, c.Name)
					default:
						panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
					}
				} else {
					switch {
					case strings.HasPrefix(c.Typ, "var") || strings.HasPrefix(c.Typ, "text"):
						n, k := obf("obfuscate_text")
						v = fmt.Sprintf(`_dymium.%s(%s,%s,0,false) AS %s`,
							n, k, hName, c.Name)
					case strings.HasPrefix(c.Typ, "char") || strings.HasPrefix(c.Typ, "bpchar"):
						n, k := obf("obfuscate_text")
						v = fmt.Sprintf(`_dymium.%s(%s,%s,0,true) AS %s`,
							n, k, hName, c.Name)
					case strings.HasPrefix(c.Typ, "uuid"):
						n, k := obf("obfuscate_uuid")
						v = fmt.Sprintf(`_dymium.%s(%s,%s) AS %s`,
							n, k, hName, c.Name)
					default:
						panic(fmt.Sprintf("Unsupported obfuscation for [%s]", c.Typ))
					}
				}
				viewDef = append(viewDef, v)
			case types.DH_Allow:
				viewDef = append(viewDef, hName+` AS `+c.Name)
			}
		}

		opts := ct_options[con.Database_type]

		hiddenTbl := "CREATE FOREIGN TABLE " + t.hiddenTableName + " (\n" +
			strings.Join(hiddenTblCols, ",\n") +
			"\n) SERVER " + serverName(con.Name) + " OPTIONS(" + opts.table(t.remoteSchema, t.remoteName) + ");\n"
		view :=
			fmt.Sprintf("CREATE VIEW %%s.%%s AS SELECT %s FROM %s;\n",
				strings.Join(viewDef, ", "),
				t.hiddenTableName)

		if err := exec(hiddenTbl); err != nil {
			return err
		}

		for _, v := range t.views {
			if err := exec(fmt.Sprintf(view, v.schema, v.name)); err != nil {
				return err
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
