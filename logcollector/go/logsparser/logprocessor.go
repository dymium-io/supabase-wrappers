package logsparser

import (
	"dymium.com/dymium/log"
	"fmt"
	aplog "github.com/apex/log"
	"os"
	"strings"
	"sync"
)

// PgLogProcessor Processor for PostgreSQL log messages (stdout)
// Expected: PostgreSql log messages in CSV format.
// Messages started with timestamp assumed to be beginning of the log event,
// without timestamp - continuation of the previous log message.
// Messages that do not match PostgreSQL CSV format are not parsed and passed
// to the colg collector "as is".
type PgLogProcessor struct {
	Parser *ParserFSM
	logStr *LogAccumulator
}

func NewPgLogProcessor() *PgLogProcessor {
	logProcessor := PgLogProcessor{logStr: newLogAccumulator()}
	parserFsm := InitParserFSM(InitPgParserStdout(logProcessor.logStr))
	logProcessor.Parser = parserFsm
	return &logProcessor
}

var once sync.Once

// PreviousProcessingState Thread-safe singleton to store previous state/massages fore parser FSM
type PreviousProcessingState struct {
	msgLine string
}

var prevState *PreviousProcessingState

func getPreviousProcessingState() *PreviousProcessingState {
	once.Do(func() {
		prevState = &PreviousProcessingState{msgLine: ""}
	})
	return prevState
}

func (prevState *PreviousProcessingState) storeState(msg string) {
	prevState.msgLine = strings.Clone(msg)
}

// ProcessMessage Message processor. Called on log events to process log message. Collects all
// multiline log strings, parse log message if it CSV PostgreSQL message and forward it
// to log collector (ex.: local_env->stdout, aws_env->kinesis).
func (processor *PgLogProcessor) ProcessMessage(line string, flushBuffer bool) {
	pgMsg, err := processor.Parser.ProcessPgLogMessage(line, FlushBufferFlag(flushBuffer))
	if err == nil {
		if pgMsg != nil {
			// Finished msg processing.
			// This is Postgres message - add meta data and forward to collector
			_, err = pgMsg.JsonString()
			if err == nil {
				// Wellformed CSV/JSON message
				PGMsgLogCollector(pgMsg)
			} else {
				// Not a CSV/JSON message format
				// TODO set default/error metadata for dlog - not PG message
				StrLogCollector("INFO", line)
			}
		}
	} else {
		if len(getPreviousProcessingState().msgLine) > 0 {
			StrLogCollector("INFO", fmt.Sprintf("%s", getPreviousProcessingState().msgLine))
		}
	}

	if len(getPreviousProcessingState().msgLine) == 0 {
		// This is the first message in the log
		if len(line) > 0 {
			// We don't need to collect empty log events
			StrLogCollector("INFO", line)
		}
	}

	// Message processing machine returns non PG messages and related error (ex. CSV parsing) one
	// step behind current message. Storing this message to avoid loosing them.
	getPreviousProcessingState().storeState(line)
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
		"FATAL":   log.FatalfCollector,
		"PANIC":   log.PanicCollector,
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

func PGMsgLogCollector(msg *PostgresLogMessage) {
	sevLevel := msg.Error_severity
	extra := aplog.Fields{
		"source":    EnvData.SourceName,
		"component": EnvData.ComponentName,
	}

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
