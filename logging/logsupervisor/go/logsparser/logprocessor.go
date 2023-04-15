package logsparser

import (
	"dymium.com/dymium/log"
	"fmt"
	aplog "github.com/apex/log"
	"os"
	"reflect"
	"strings"
	"sync"
	"time"
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
				if len(line) > 0 {
					// we don't need to collect empty log messages - ex. happens on timeout events
					StrLogCollector("INFO", line)
				}
			}
		}
	} else {
		if len(getPreviousProcessingState().msgLine) > 0 {
			if len(line) > 0 {
				// we don't need to collect empty log messages - ex. happens on timeout events
				StrLogCollector("INFO", fmt.Sprintf("%s", getPreviousProcessingState().msgLine))
			}
		}
	}

	if len(getPreviousProcessingState().msgLine) == 0 {
		// This is the first message in the log
		StrLogCollector("INFO", line)
	}

	// Message processing machine returns non PG messages and related error (ex. CSV parsing) one
	// step behind current message. Storing this message to avoid loosing them.
	getPreviousProcessingState().storeState(line)
}

func MakeLoggerFunction(f interface{}) interface{} {
	rf := reflect.TypeOf(f)
	if rf.Kind() != reflect.Func {
		fmt.Println(rf.Kind().String())
		return nil
		//panic("expects a function")
	}
	vf := reflect.ValueOf(f)
	wrapperF := reflect.MakeFunc(rf, func(in []reflect.Value) []reflect.Value {
		out := vf.Call(in)
		return out
	})
	return wrapperF.Interface()
}

func LogCollectorWithFields(severity string, fields aplog.Fields, data string) {
	severityToLoggerMap := map[string]interface{}{
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

	loggerName, loggerExists := severityToLoggerMap[severity]
	if !loggerExists {
		fmt.Fprintf(os.Stderr, fmt.Sprintf("Logger function for %s does not exist.", severity))
	} else {
		if len(data) > 0 {
			fields["message"] = data
		}
		logger := MakeLoggerFunction(loggerName).(func(aplog.Fields, string))
		if logger != nil {
			logger(fields, " ")
		} else {
			// in case when there is no matching log func - what should we do? For now set it to INFO
			logger := MakeLoggerFunction(severityToLoggerMap["INFO"]).(func(aplog.Fields, string))
			if logger == nil {
				// We should never get here since Infof logger is part of the package dymium.log
				fmt.Fprintf(os.Stderr, fmt.Sprintf("Logger %s function does not exist.", loggerName))
			} else {
				logger(fields, " ")
			}
		}
	}
}

func PGMsgLogCollector(msg *PostgresLogMessage) {
	sevLevel := msg.Error_severity
	extra := aplog.Fields{
		"eventtime":  msg.Log_time,
		"component":  "logsupervisor", // TODO what should be the component supervisor/PgGaurdian/Postgres?
		"dbname":     msg.Database_name,
		"session":    msg.Session_id,
		"primarykey": fmt.Sprintf("%s:%d", msg.Session_id, msg.Session_line_num),
	}

	data, _ := msg.JsonString() // Ignoring the error, this func must be called only when msg is wellformed
	LogCollectorWithFields(sevLevel, extra, data)
}

func StrLogCollector(severity string, data string) {
	extra := aplog.Fields{
		"eventtime": time.Now().Format("2023-04-04 03:20:24.193 UTC"), // add current timestamp
		"component": "logsupervisor",                                  // TODO what should be the component supervisor/PgGaurdian/Postgres?
		"message":   data,
	}
	LogCollectorWithFields(severity, extra, data)
}
