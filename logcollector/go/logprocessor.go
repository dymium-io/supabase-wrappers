package main

import (
	"dymium.com/dymium/log"
	"encoding/csv"
	"encoding/json"
	"errors"
	"fmt"
	aplog "github.com/apex/log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func ProcessMessage(msg string) {
	if msg == "\n" {
		// skip empty lines
		return
	}
	logmsg, err := ParseLogCSV(msg)
	if err != nil {
		StrLogCollector("INFO", msg)
	} else {
		PGMsgLogCollector(logmsg)
	}
}

// PostgresLogMessage Log message structure based on PostrgreSQL documentation
// See: https://www.postgresql.org/docs/14/runtime-config-logging.html#RUNTIME-CONFIG-LOGGING-CSVLOG
// PRIMARY KEY (session_id, session_line_num)
type PostgresLogMessage struct {
	Log_time               string `json:"Log_Time"`               //timestamp(3) with time zone
	User_name              string `json:"User_Name"`              // text
	Database_name          string `json:"Database_Name"`          // text
	Process_id             int    `json:"Process_Id"`             // integer
	Connection_from        string `json:"Connection_From"`        // text
	Session_id             string `json:"Session_Id"`             // text
	Session_line_num       uint64 `json:"Session_Line_Num"`       // bigint
	Command_tag            string `json:"Command_Tag"`            // text
	Session_start_time     string `json:"Session_Start_Time"`     // timestamp with time zone
	Virtual_transaction_id string `json:"Virtual_Transaction_Id"` // text
	Transaction_id         uint64 `json:"Transaction_Id"`         // bigint
	Error_severity         string `json:"Error_Severity"`         // text
	Sql_state_code         string `json:"Sql_State_Code"`         // text,
	Message                string `json:"Message"`                // text,
	Detail                 string `json:"Detail"`                 // text,
	Hint                   string `json:"Hint"`                   // text,
	Internal_query         string `json:"Internal_Query"`         // text,
	Internal_query_pos     int    `json:"Internal_Query_Pos"`     // integer,
	Context                string `json:"Context"`                // text,
	Query                  string `json:"Query"`                  // text,
	Query_pos              int    `json:"Query_Pos"`              // integer,
	Location               string `json:"Location"`               // text,
	Application_name       string `json:"Application_Name"`       // text,
	Backend_type           string `json:"Backend_Type"`           // text,
	Leader_pid             int    `json:"Leader_Pid"`             // integer,
	Query_id               uint64 `json:"Query_Id"`               // bigint,
}

func (message *PostgresLogMessage) JsonString() (string, error) {
	jsonData, err := json.Marshal(message)
	if err != nil {
		return "", err
	}
	msg := string(jsonData)
	return msg, nil
}

func ParseLogCSV(msg string) (*PostgresLogMessage, error) {
	r := csv.NewReader(strings.NewReader(msg))
	record, err := r.Read()
	if err != nil || len(record) != 26 {
		return nil, errors.New("Message doesn't match PG csv log message structure.")
	}

	pgLogMessage := new(PostgresLogMessage)

	pgLogMessage.Log_time = record[0]
	pgLogMessage.User_name = record[1]
	pgLogMessage.Database_name = record[2]
	pgLogMessage.Process_id, err = strconv.Atoi(record[3])
	if err != nil {
		if len(record[3]) > 0 {
			return nil, err
		}
		pgLogMessage.Process_id = 0
	}
	pgLogMessage.Connection_from = record[4]
	pgLogMessage.Session_id = record[5]
	pgLogMessage.Session_line_num, err = strconv.ParseUint(record[6], 10, 64)
	if err != nil {
		if len(record[6]) > 0 {
			return nil, err
		}
		pgLogMessage.Session_line_num = 0
	}
	pgLogMessage.Command_tag = record[7]
	pgLogMessage.Session_start_time = record[8]
	pgLogMessage.Virtual_transaction_id = record[9]
	pgLogMessage.Transaction_id, err = strconv.ParseUint(record[10], 10, 64)
	if err != nil {
		if len(record[10]) > 0 {
			return nil, err
		}
		pgLogMessage.Transaction_id = 0
	}
	pgLogMessage.Error_severity = record[11]
	pgLogMessage.Sql_state_code = record[12]
	pgLogMessage.Message = ObfuscatePasswords(record[13])
	pgLogMessage.Detail = record[14]
	pgLogMessage.Hint = record[15]
	pgLogMessage.Internal_query = record[16]
	pgLogMessage.Internal_query_pos, err = strconv.Atoi(record[17])
	if err != nil {
		if len(record[17]) > 0 {
			return nil, err
		}
		pgLogMessage.Internal_query_pos = 0
	}
	pgLogMessage.Context = record[18]
	pgLogMessage.Query = record[19]
	pgLogMessage.Query_pos, err = strconv.Atoi(record[20])
	if err != nil {
		if len(record[20]) > 0 {
			return nil, err
		}
		pgLogMessage.Query_pos = 0
	}
	pgLogMessage.Location = record[21]
	pgLogMessage.Application_name = record[22]
	pgLogMessage.Backend_type = record[23]
	pgLogMessage.Leader_pid, err = strconv.Atoi(record[24])
	if err != nil {
		if len(record[24]) > 0 {
			return nil, err
		}
		pgLogMessage.Leader_pid = 0
	}
	pgLogMessage.Query_id, err = strconv.ParseUint(record[25], 10, 64)
	if err != nil {
		if len(record[25]) > 0 {
			return nil, err
		}
		pgLogMessage.Query_id = 0
	}
	return pgLogMessage, nil
}

// List of statements which contain passwords:
var reStatements = []*regexp.Regexp{
	regexp.MustCompile(`((?i)statement:[\s\r\n]+CREATE[\s\r\n]+USER[\s\r\n]+MAPPING[\s\r\n]+FOR[\s\r\n]+.*[\s\r\n]+SERVER[\s\r\n]+.*[\s\r\n]+OPTIONS[\s\r\n]+\(.*password\s+['"])(\S+)((?s)['"].*\).*$)`),
	regexp.MustCompile(`((?i)statement:[\s\r\n]+CREATE[\s\r\n]+USER[\s\r\n]+.*[\s\r\n]+PASSWORD[\s\r\n]+['"])(\S+)((?s)['"].*$)`),
	regexp.MustCompile(`((?i)statement:[\s\r\n]+ALTER[\s\r\n]+USER[\s\r\n]+.*?WITH[\s\r\n]+.*PASSWORD[\s\r\n]+['"])(\S+)((?s)['"].*$)`),
	regexp.MustCompile(`((?i)\s*insert_secret\s*\(\s*'vault_access_key_id'\s*,\s*')(.*?)('\s*\)\s* INTO \s*key_id\s*)`),
	regexp.MustCompile(`((?i)\s*insert_secret\s*\(\s*'vault_secret_key_id'\s*,\s*')(.*?)('\s*\)\s* INTO \s*secret_key_id\s*)`),
}

// ObfuscatePasswords replaces passwords in the message with the string "********"
func ObfuscatePasswords(msg string) string {
	for _, re := range reStatements {
		if re.MatchString(msg) {
			msg = re.ReplaceAllString(msg, "$1********$3")
		}
	}
	return msg
}

var EnvData struct {
	ComponentName string
	SourceName    string
	Tenant        string
	Session       string
	User          string
}

type LogFunc func(string, string, string, aplog.Fields, string)

func LogCollectorWithFields(severity string, fields aplog.Fields, session string, username string, msg string) {
	severityToLoggerMap := map[string]LogFunc{
		"DEBUG1":  log.DebugfCollector,
		"DEBUG2":  log.DebugfCollector,
		"DEBUG3":  log.DebugfCollector,
		"DEBUG4":  log.DebugfCollector,
		"DEBUG5":  log.DebugfCollector,
		"INFO":    log.InfofCollector,
		"NOTICE":  log.InfofCollector,
		"WARNING": log.WarnfCollector,
		"LOG":     log.InfofCollector,
		"ERROR":   log.ErrorfCollector,
		// using fatal or panic logger kills the logcollector
		"FATAL": log.ErrorfCollector,
		"PANIC": log.ErrorfCollector,
	}

	logger, loggerExists := severityToLoggerMap[severity]
	if !loggerExists {
		_, _ = fmt.Fprintf(os.Stderr, fmt.Sprintf("Logger function for %s does not exist.", severity))
	} else {
		if logger != nil {
			logger(EnvData.Tenant, session, username, fields, msg)
		} else {
			logger = severityToLoggerMap["INFO"]
			if logger == nil {
				// We should never get here since Infof logger is part of the package dymium.log
				_, _ = fmt.Fprintf(os.Stderr, fmt.Sprintf("Logger for %s does not exist.", severity))
			} else {
				logger(EnvData.Tenant, session, username, fields, msg)
			}
		}
	}
}

// List of INTERNAL statements:
// statement: SELECT
var selectStatement = regexp.MustCompile(`((?i).*:[\s\r\n]+SELECT[\s\r\n]+)`)

// FROM _dymium
var fromDymium = regexp.MustCompile(`((?i)[\s\r\n]+FROM[\s\r\n]+_dymium)`)

// FROM information_schema
var fromInfoSchema = regexp.MustCompile(`((?i)[\s\r\n]+FROM[\s\r\n]+information_schema)`)

// FROM pg_*
var fromPGSchema = regexp.MustCompile(`((?i)[\s\r\n]+FROM[\s\r\n]+pg_.*)`)

func tagSQLStatement(msg *PostgresLogMessage) string {
	// We can also add msg.Connection_from if we have a list of internal IPs
	if msg.User_name != "postgres" && msg.User_name != "dymium" {
		if selectStatement.MatchString(msg.Message) {
			if !fromDymium.MatchString(msg.Message) {
				if !fromInfoSchema.MatchString(msg.Message) {
					if !fromPGSchema.MatchString(msg.Message) {
						return "C"
					}
				}
			}
		}
	}
	return "INT"
}

func PGMsgLogCollector(msg *PostgresLogMessage) {
	sevLevel := msg.Error_severity
	extra := aplog.Fields{
		"timestamp": msg.Log_time,
		"source":    EnvData.SourceName,
		"component": EnvData.ComponentName,
	}

	stTag := tagSQLStatement(msg)
	extra["stTag"] = stTag
	data, _ := msg.JsonString() // Ignoring the error, this func must be called only when msg is wellformed
	LogCollectorWithFields(sevLevel, extra, msg.Session_id, msg.User_name, data)
}

func StrLogCollector(severity string, msg string) {
	extra := aplog.Fields{
		"source":    EnvData.SourceName,
		"component": EnvData.ComponentName,
	}
	LogCollectorWithFields(severity, extra, "", "", msg)
}
