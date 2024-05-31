package DbSetup

import (
	"bytes"
	"embed"
	"fmt"
	"strings"
	"text/template"

	"dymium.io/DbSetup/types"
)

//go:embed templates/*
var templateFS embed.FS

func init() {
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
	var _ = define_rust_ext

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
		ext:        define_ext("tds_fdw"),
		server_def: define_server("tds.sql"),
		table: func(remoteSchema, remoteTable string) string {
			return fmt.Sprintf("schema_name '%s', table_name '%s'",
				esc(remoteSchema), esc(remoteTable))
		},
		// ext:        define_rust_ext("mssql_wrapper", "mssql_fdw_handler", "mssql_fdw_validator"),
		// server_def: define_server("mssql.sql"),
		// table: func(remoteSchema, remoteTable string) string {
		//	return fmt.Sprintf("table '[%s].[%s]'",
		//		remoteSchema, remoteTable)
		// },
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
	ct_options[types.CT_S3] = ct_option{
		ext:        define_rust_ext("s3_wrapper", "s3_fdw_handler", "s3_fdw_validator"),
		server_def: define_server("s3.sql"),
		table: func(remoteSchema, remoteTable string) string {
			//uri 's3://bucket/s3_table.csv',
			//    format 'csv',
			//    has_header 'true'
			uri := fmt.Sprintf("s3://%s/%s", remoteSchema, remoteTable)
			if strings.HasSuffix(remoteTable, ".csv") {
				return fmt.Sprintf("uri '%s', format 'csv', has_header 'true'", uri)
			}
			if strings.HasSuffix(remoteTable, ".json") {
				return fmt.Sprintf("uri '%s', format 'jsonl'", uri)
			}
			// Parquet is the default - TODO: should be throw an error here or it's checked before?
			return fmt.Sprintf("uri '%s', format 'parquet'", uri)
		},
	}
}
