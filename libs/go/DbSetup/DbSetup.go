package DbSetup

import (
	"dymium.com/dymium/log"

	"bytes"
	"text/template"

	"context"
	"fmt"
	"regexp"
	"strings"

	"embed"

	"database/sql"

	"crypto/sha256"

	"DbSetup/types"
)

//go:embed templates/*
var templateFS embed.FS

var obf_funcs func() []string
var obf func(name string) (string, string)

var redact_value func(t string) string

func gen_redact_value_func() {
	redactDefs := []string{`^interval                                 => '0 days'`,
		`^(char|var|bpchar).*\((1|2)\)                            => ''`,
		`^(char|var|text|bpchar)                                  => 'xxx'`,
		`^(bigint|int|smallint|double|float|real|decimal|numeric) => 0`,
		//
		`^bool                                                    => false`,
		`^xml                                                     => ''`,
		`bytea                                                    => E'\x'`,
		//
		`^jsonb?                                                  => '{}'`,
		`^uuid                                                    => '00000000-0000-0000-0000-000000000000'`,
		//
		`^timestamp.*with +time *zone                             => '2000-01-01 00:00:00 UTC'`,
		`^timestamp                                               => '0001-01-01 00:00:00'`,
		//
		`^time.*with +time *zone                                  => '00:00:00 UTC'`,
		`^time                                                    => '00:00:00'`,
		//
		`^date                                                    => '0001-01-01'`,
		`^money                                                   => 0`,
		//
		`^point                                                   => '(0,0)'`,
		`^line                                                    => '(0,0,0)'`,
		//
		`^(lseg|box|path)                                         => '((0,0), (1,1))'`,
		`^polygon                                                 => '((0,0), (0,1), (1,0))'`,
		//
		`^circle                                                  => '<(0,0), 1>'`,
		`^(inet|cidr)                                             => '0.0.0.0/0'`,
		//
		`^macaddr8                                                => '00:00:00:00:00:00:00:00'`,
		`^macaddr                                                 => '00:00:00:00:00:00'`,
		//
		`^bit                                                     => B'0'`,
	}
	spl := regexp.MustCompile(` *=> *`)
	type regexpMap struct {
		r *regexp.Regexp
		t string
	}
	redacts := make([]regexpMap, 0, len(redactDefs))
	for _, r := range redactDefs {
		ri := spl.Split(r, 2)
		redacts = append(redacts,
			regexpMap{
				r: regexp.MustCompile(`(?i:` + ri[0] + `)`),
				t: string(ri[1]),
			})
	}
	redact_value = func(t string) string {
		for _, ri := range redacts {
			if ri.r.MatchString(t) {
				return ri.t
			}
		}
		return `''`
	}
}

type ct_option struct {
	ext        string
	server_def func(server, address string, port int, database string, use_tls bool, user string, password string) (string, error)
	table      func(remoteSchema, remoteTable string) string
}

var ct_options map[types.ConnectionType]ct_option

func init() {
	gen_obf_func()
	gen_redact_value_func()

	define_ext := func(e string) string {
		return "CREATE EXTENSION IF NOT EXISTS " + e + " WITH SCHEMA public"
	}
	define_rust_ext := func(e, h, v string) string {
		return `DO $$
                        BEGIN
		          IF NOT EXISTS (
			     SELECT 1
			     FROM pg_catalog.pg_foreign_data_wrapper
			     WHERE fdwname = '` + e + `'
		          ) THEN
		             EXECUTE 'CREATE FOREIGN DATA WRAPPER ` + e + ` HANDLER ` + h + ` VALIDATOR ` + v + `;';
		          END IF;
                        END $$`
	}

	define_server := func(template_name string) func(server, address string, port int, database string, use_tls bool, user string, password string) (string, error) {
		tmpl := template.Must(template.ParseFS(templateFS, "templates/"+template_name))
		return func(server, address string, port int, database string, use_tls bool, user string, password string) (string, error) {
			type server_params struct {
				Server   string
				Address  string
				Port     int
				Database string
				Use_tls  bool
				User     string
				Password string
			}
			var buf bytes.Buffer
			if err := tmpl.Execute(&buf, server_params{
				Server:   esc(server),
				Address:  esc(address),
				Port:     port,
				Database: esc(database),
				Use_tls:  use_tls,
				User:     esc(user),
				Password: esc(password),
			}); err != nil {
				return "", err
			}
			return buf.String(), nil
		}
	}

	ct_options = make(map[types.ConnectionType]ct_option)

	ct_options[types.CT_PostgreSQL] = ct_option{
		ext:        define_ext("postgres_fdw"),
		server_def: define_server("postgres.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("schema_name '%s', table_name '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
	ct_options[types.CT_MySQL] = ct_option{
		ext:        define_ext("mysql_fdw"),
		server_def: define_server("mysql.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("dbname '%s', table_name '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
	ct_options[types.CT_MariaDB] = ct_options[types.CT_MySQL]
	ct_options[types.CT_OracleDB] = ct_option{
		ext:        define_ext("oracle_fdw"),
		server_def: define_server("oracle.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("schema '%s', table '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
	ct_options[types.CT_DB2] = ct_option{
		ext:        define_ext("db2_fdw"),
		server_def: define_server("db2.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("schema '%s', table '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
	ct_options[types.CT_SqlServer] = ct_option{
		ext:        define_rust_ext("mssql_wrapper", "mssql_fdw_handler", "mssql_fdw_validator"),
		server_def: define_server("mssql.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("table '[%s].[%s]'",
				remoteSchema, remoteTable)
		},
	}
	ct_options[types.CT_MongoDB] = ct_option{
		ext:        define_ext("mongo_fdw"),
		server_def: define_server("mongodb.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("database '%s', collection '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
	ct_options[types.CT_Elasticsearch] = ct_option{
		ext:        define_ext("jdbc_fdw"),
		server_def: define_server("es.sql"),
		table: func(remoteSchema, remoteTable string) string {
			if remoteSchema == "_defaultdb_" {
				return fmt.Sprintf("table_name '%s'", esc(remoteTable))
			}
			return fmt.Sprintf("schema_name '%s', table_name '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
	}
}

func ConfigureDatabase(db *sql.DB,
	datascope *types.Scope,
	connections map[string]types.Connection,
	credentials map[string]types.Credential,
	createDymiumTables bool) error {

	log.Infof("configureDatabase: datascope=%+v\n", datascope)
	log.Infof("configureDatabase: connections=%+v\n", connections)

	localUser := fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(datascope.Name+"_dymium")))

	connectionTypes := map[types.ConnectionType]struct{}{}
	for k := range datascope.Connections {
		connectionTypes[connections[datascope.Connections[k]].Database_type] = struct{}{}
	}

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

	exec := func(sql string) error {
		log.Infof(sql)
		if _, err := tx.ExecContext(ctx, sql); err != nil {
			return rollback(err, "["+sql+"] failed")
		}
		return nil
	}

	if err = exec(`CREATE EXTENSION IF NOT EXISTS supabase_vault CASCADE`); err != nil {
		return err
	}
	{
		if err := exec("GRANT USAGE ON SCHEMA vault TO " + localUser); err != nil {
			return err
		}
		if err := exec(`DO $$
				DECLARE
                                    uname text := '` + localUser + `';
				    tbl_name text;
				BEGIN
                                    EXECUTE format('GRANT USAGE ON SCHEMA vault TO %I;', uname);
                                    EXECUTE format('GRANT EXECUTE ON FUNCTION pgsodium.crypto_aead_det_decrypt(bytea, bytea, uuid, bytea) TO %I;',uname);
                                    CREATE EXTENSION IF NOT EXISTS wrappers WITH SCHEMA public;
                                    EXECUTE format('GRANT SELECT, INSERT, UPDATE ON wrappers_fdw_stats TO %I;',uname);
				    FOR tbl_name IN SELECT table_name FROM information_schema.tables WHERE table_schema = 'vault'
				    LOOP
					EXECUTE format('GRANT SELECT ON vault.%I TO %I;', tbl_name, uname);
				    END LOOP;
				END$$;`); err != nil {
			return err
		}
	}

	if err = exec(`
          CREATE OR REPLACE FUNCTION insert_secret(p_name text, p_secret text) RETURNS UUID AS $$
          DECLARE
              v_id UUID;
          BEGIN
             -- Attempt to update first and capture the id
             UPDATE vault.secrets
             SET secret = p_secret
             WHERE name = p_name
             RETURNING key_id INTO v_id;

             -- If the update did not affect any rows, then insert
             IF NOT FOUND THEN
                 INSERT INTO vault.secrets (name, secret)
                 VALUES (p_name, p_secret)
                 RETURNING key_id INTO v_id;
             END IF;

             -- Return the id of the affected row
             RETURN v_id;
         END;
         $$
         LANGUAGE plpgsql`); err != nil {
		return err
	}

	for ct := range connectionTypes {
		if err = exec(ct_options[ct].ext); err != nil {
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
		cred, ok := credentials[c.Id]
		if !ok {
			return rollback(fmt.Errorf("misconfiguration!"),
				fmt.Sprintf("Can not find credentials for "+c.Id))
		}

		if sql, err := ct_options[c.Database_type].server_def(serverName(c.Name), c.Address, c.Port, c.Dbname, c.Use_tls, cred.User_name, cred.Password); err != nil {
			return rollback(err, fmt.Sprintf("server_def(%q, %q, %d, %q, %v, %q, \"******\"", serverName(c.Name), c.Address, c.Port, c.Dbname, c.Use_tls, cred.User_name, cred.Password))
		} else if _, err := tx.ExecContext(ctx, sql); err != nil {
			return rollback(err, "["+sql+"] failed")
		}

		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.servers (server) VALUES ( $1 )", serverName(c.Name)); err != nil {
			return rollback(err, "Registering server "+serverName(c.Name)+" failed")
		}
	}

	for k := range shortSchemas {
		// k is already PostgresEscaped!
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
			return rollback(err, "Registering schema "+k+" failed")
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
		if _, err := tx.ExecContext(ctx, "INSERT INTO _dymium.schemas (\"schema\") VALUES ( $1 )", kk); err != nil {
			return rollback(err, "Registering schema "+kk+" failed")
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

func esc(str string) string {
	return ParamEscape(str)
}

func serverName(str string) string {
	return str + "_server"
}

func findDuplicates[T any](items []T, getName func(T) string) []bool {
	nameCount := make(map[string]int)
	lowered := make([]string, len(items))

	for k, item := range items {
		name := getName(item)
		lowered[k] = strings.ToLower(name)
		nameCount[lowered[k]]++
	}

	result := make([]bool, len(items))
	for k := range result {
		result[k] = nameCount[lowered[k]] > 1
	}

	return result
}

func gen_obf_func() {
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
