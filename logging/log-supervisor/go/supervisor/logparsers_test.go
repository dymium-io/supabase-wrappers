package supervisor

import (
	"github.com/looplab/fsm"
	"reflect"
	"testing"
)

func TestParserFMS_parseSDTDOU(t *testing.T) {
	normalMessage := "2023-04-04 03:27:48.567 UTC,\"postgres\",\"postgres\",39," +
		"\"172.18.0.1:37664\",642b9934.27,6,\"idle\",2023-04-04 03:27:48 UTC,3/28,0,LOG,00000," +
		"\"statement: CREATE TABLE \"\"test accounts\"\" (\n\tuser_id serial PRIMARY KEY," +
		"\n\tusername VARCHAR ( 50 ) UNIQUE NOT NULL,\n\tpassword VARCHAR ( 50 ) NOT NULL," +
		"\n\temail VARCHAR ( 255 ) UNIQUE NOT NULL,\n\tcreated_on TIMESTAMP NOT NULL," +
		"\n        last_login TIMESTAMP \n);\",,,,,,,,,\"pgAdmin 4 - CONN:9342839\",\"client backend\",,0\n"

	normalMsgStruct := PostgresLogMessage{
		Log_time:               "2023-04-04 03:27:48.567 UTC",
		User_name:              "postgres",
		Database_name:          "postgres",
		Process_id:             39,
		Connection_from:        "172.18.0.1:37664",
		Session_id:             "642b9934.27",
		Session_line_num:       6,
		Command_tag:            "idle",
		Session_start_time:     "2023-04-04 03:27:48 UTC",
		Virtual_transaction_id: "3/28",
		Transaction_id:         0,
		Error_severity:         "LOG",
		Sql_state_code:         "00000",
		Message: "statement: CREATE TABLE \"test accounts\" " +
			"(\n\tuser_id serial PRIMARY KEY,\n\tusername VARCHAR ( 50 ) " +
			"UNIQUE NOT NULL,\n\tpassword VARCHAR ( 50 ) NOT NULL,\n" +
			"\temail VARCHAR ( 255 ) UNIQUE NOT NULL,\n" +
			"\tcreated_on TIMESTAMP NOT NULL,\n        last_login TIMESTAMP \n);",
		Detail:             "",
		Hint:               "",
		Internal_query:     "",
		Internal_query_pos: 0,
		Context:            "",
		Query:              "",
		Query_pos:          0,
		Location:           "",
		Application_name:   "pgAdmin 4 - CONN:9342839",
		Backend_type:       "client backend",
		Leader_pid:         0,
		Query_id:           0,
	}

	type fields struct {
		fsm *fsm.FSM
	}
	type args struct {
		logMessage string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *PostgresLogMessage
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name: "Normal log message. Should return correct json log message",
			args: args{normalMessage},
			want: &normalMsgStruct,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parser := &ParserFSM{}
			got, err := parser.parseStdoutMsg(tt.args.logMessage)
			if (err != nil) != tt.wantErr {
				t.Errorf("parseStdoutMsg() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf(
					"parseStdoutMsg() got = %v, want %v",
					got,
					tt.want,
				)
			}
		})
	}
}

func TestPostgresLogMessage_toJsonString(t *testing.T) {
	normalMessage := "{\"Log_time\":\"2023-04-04 03:27:48.567 UTC\",\"User_name\":\"postgres\"," +
		"\"Database_name\":\"postgres\",\"Process_id\":39,\"Connection_from\":\"172.18.0.1:37664\"," +
		"\"Session_id\":\"642b9934.27\",\"Session_line_num\":6,\"Command_tag\":\"idle\"," +
		"\"Session_start_time\":\"2023-04-04 03:27:48 UTC\",\"Virtual_transaction_id\":\"3/28\"," +
		"\"Transaction_id\":0,\"Error_severity\":\"LOG\",\"Sql_state_code\":\"00000\"," +
		"\"Message\":\"statement: CREATE TABLE \\\"test accounts\\\" " +
		"(\\n\\tuser_id serial PRIMARY KEY,\\n\\tusername VARCHAR ( 50 ) UNIQUE NOT NULL," +
		"\\n\\tpassword VARCHAR ( 50 ) NOT NULL,\\n\\temail VARCHAR ( 255 ) UNIQUE NOT NULL," +
		"\\n\\tcreated_on TIMESTAMP NOT NULL,\\n        last_login TIMESTAMP \\n);\",\"Detail\":\"\"," +
		"\"Hint\":\"\",\"Internal_query\":\"\",\"Internal_query_pos\":0,\"Context\":\"\"," +
		"\"Query\":\"\",\"Query_pos\":0,\"Location\":\"\"," +
		"\"Application_name\":\"pgAdmin 4 - CONN:9342839\"," +
		"\"Backend_type\":\"client backend\",\"Leader_pid\":0,\"Query_id\":0}"

	normalMsgStruct := PostgresLogMessage{
		Log_time:               "2023-04-04 03:27:48.567 UTC",
		User_name:              "postgres",
		Database_name:          "postgres",
		Process_id:             39,
		Connection_from:        "172.18.0.1:37664",
		Session_id:             "642b9934.27",
		Session_line_num:       6,
		Command_tag:            "idle",
		Session_start_time:     "2023-04-04 03:27:48 UTC",
		Virtual_transaction_id: "3/28",
		Transaction_id:         0,
		Error_severity:         "LOG",
		Sql_state_code:         "00000",
		Message: "statement: CREATE TABLE \"test accounts\" " +
			"(\n\tuser_id serial PRIMARY KEY,\n\tusername VARCHAR ( 50 ) " +
			"UNIQUE NOT NULL,\n\tpassword VARCHAR ( 50 ) NOT NULL,\n" +
			"\temail VARCHAR ( 255 ) UNIQUE NOT NULL,\n" +
			"\tcreated_on TIMESTAMP NOT NULL,\n        last_login TIMESTAMP \n);",
		Detail:             "",
		Hint:               "",
		Internal_query:     "",
		Internal_query_pos: 0,
		Context:            "",
		Query:              "",
		Query_pos:          0,
		Location:           "",
		Application_name:   "pgAdmin 4 - CONN:9342839",
		Backend_type:       "client backend",
		Leader_pid:         0,
		Query_id:           0,
	}

	type args struct {
		logMessage PostgresLogMessage
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name: "Normal log message. Should return correct json log message",
			args: args{normalMsgStruct},
			want: normalMessage,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := normalMsgStruct.toJsonString()
			if (err != nil) != tt.wantErr {
				t.Errorf("toJsonString() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("toJsonString() got = %v, want %v", got, tt.want)
			}
		})
	}
}
