package main

import (
	"testing"
)

func TestObfuscatePasswords(t *testing.T) {
	type args struct {
		msg string
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			name: "Obfuscate passwords in 'CREATE USER MAPPING' statement",
			args: args{
				msg: "statement: CREATE USER MAPPING FOR _3381fa486e704495c5e6e9c38aa2e5a6_ SERVER maria_server OPTIONS (username 'root', password '1234')",
			},
			want: "statement: CREATE USER MAPPING FOR _3381fa486e704495c5e6e9c38aa2e5a6_ SERVER maria_server OPTIONS (username 'root', password '********')",
		},
		{
			name: "Obfuscate passwords in 'CREATE USER MAPPING' statement with tabs/spaces/newlines",
			args: args{
				msg: "statement:\n\t\t                                      CREATE USER MAPPING FOR _f2cd3c3be7d6e548f15d02c1c1843861_\n\t\t                                      SERVER mysql_server\n\t\t                                      OPTIONS (username 'root', password '1234')",
			},
			want: "statement:\n\t\t                                      CREATE USER MAPPING FOR _f2cd3c3be7d6e548f15d02c1c1843861_\n\t\t                                      SERVER mysql_server\n\t\t                                      OPTIONS (username 'root', password '********')",
		},

		{
			name: "Obfuscate passwords in 'CREATE USER SUPERUSER' statement",
			args: args{
				msg: "statement: CREATE USER username SUPERUSER PASSWORD 'xsdTY7MRpv*LoGmBXMez'",
			},
			want: "statement: CREATE USER username SUPERUSER PASSWORD '********'",
		},
		{
			name: "Obfuscate passwords in 'CREATE USER' statement",
			args: args{
				msg: "statement: CREATE USER username PASSWORD 'xsdTY7MRpv*LoGmBXMez'",
			},
			want: "statement: CREATE USER username PASSWORD '********'",
		},
		{
			name: "Obfuscate passwords in 'ALTER USER' statement, case 1",
			args: args{
				msg: "statement: ALTER USER edison WITH ENCRYPTED PASSWORD '=UOBAgxY9d'",
			},
			want: "statement: ALTER USER edison WITH ENCRYPTED PASSWORD '********'",
		},
		{
			name: "Obfuscate passwords in 'ALTER USER' statement, case 2",
			args: args{
				msg: "statement: ALTER USER edison WITH PASSWORD '=UOBAgxY9d'",
			},
			want: "statement: ALTER USER edison WITH PASSWORD '********'",
		},
		{
			name: "Do not obfuscate passwords in other statements",
			args: args{
				msg: "statement: CREATE FOREIGN TABLE Person.Password (\n  " +
					"BusinessEntityID integer OPTIONS( redact '17' ) NOT NULL,\n " +
					" PasswordHash varchar(128) OPTIONS( redact '10' ) NOT NULL,\n  " +
					"PasswordSalt varchar(10) OPTIONS( redact '10' ) NOT NULL,\n  " +
					"ModifiedDate timestamp (3) without time zone OPTIONS( redact '65' ) " +
					"NOT NULL\n)\n  SERVER sql_server_adventureworks_server " +
					"OPTIONS(schema_name 'Person', table_name 'Password')",
			},
			want: "statement: CREATE FOREIGN TABLE Person.Password (\n  " +
				"BusinessEntityID integer OPTIONS( redact '17' ) NOT NULL,\n " +
				" PasswordHash varchar(128) OPTIONS( redact '10' ) NOT NULL,\n  " +
				"PasswordSalt varchar(10) OPTIONS( redact '10' ) NOT NULL,\n  " +
				"ModifiedDate timestamp (3) without time zone OPTIONS( redact '65' ) " +
				"NOT NULL\n)\n  SERVER sql_server_adventureworks_server " +
				"OPTIONS(schema_name 'Person', table_name 'Password')",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := ObfuscatePasswords(tt.args.msg); got != tt.want {
				t.Errorf("ObfuscatePasswords() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_tagSQLStatement(t *testing.T) {
	type args struct {
		msg *PostgresLogMessage
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			name: "Not select statement",
			args: args{
				msg: &PostgresLogMessage{User_name: "user", Message: "statement: ALTER USER edison WITH PASSWORD '=UOBAgxY9d'"},
			},
			want: "INT",
		},
		{
			name: "Select statement from information_schema",
			args: args{
				msg: &PostgresLogMessage{User_name: "user", Message: "statement: select * from information_schema.tables"},
			},
			want: "INT",
		},
		{
			name: "Select statement from _dymium schema",
			args: args{
				msg: &PostgresLogMessage{User_name: "user", Message: "statement: select * from _dymium.dymium"},
			},
			want: "INT",
		},
		{
			name: "Select statement from pg_* schema",
			args: args{
				msg: &PostgresLogMessage{User_name: "user", Message: "statement: select * from pg_dymium"},
			},
			want: "INT",
		},
		{
			name: "Select statement from customer schema, customer username",
			args: args{
				msg: &PostgresLogMessage{User_name: "user", Message: "statement: select * from fdw.customer"},
			},
			want: "C",
		},
		{
			name: "Select statement from customer schema, dymium username",
			args: args{
				msg: &PostgresLogMessage{User_name: "dymium", Message: "statement: select * from fdw.customer"},
			},
			want: "INT",
		},
		{
			name: "Select statement from customer schema, postgres username",
			args: args{
				msg: &PostgresLogMessage{User_name: "postgres", Message: "statement: select * from fdw.customer"},
			},
			want: "INT",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tagSQLStatement(tt.args.msg); got != tt.want {
				t.Errorf("tagSQLStatement() = %v, want %v", got, tt.want)
			}
		})
	}
}
