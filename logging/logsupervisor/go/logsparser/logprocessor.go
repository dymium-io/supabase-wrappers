package logsparser

import (
	"fmt"
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
	var logMsg string

	pgMsg, err := processor.Parser.ProcessPgLogMessage(line, FlushBufferFlag(flushBuffer))
	if err == nil {
		if pgMsg != nil {
			// Finished msg processing.
			// This is Postgres message - add meta data and forward to collector
			logMsg, err = pgMsg.JsonString()
			if err == nil {
				// Wellformed CSV/JSON message
				// TODO create metadata for the log based on env vars and pgMsg struct
				line = logMsg
			} else {
				// Not a CSV/JSON message format
				// TODO set default/error metadata for dlog - not PG message
				logMsg = line
			}
		}
	} else {
		// TODO set default/error metadata for dlog
		if len(getPreviousProcessingState().msgLine) > 0 {
			fmt.Println(getPreviousProcessingState().msgLine)
		}
	}
	// TODO based on metadata and logSeverity->logFunc map forward message to log collector
	if len(logMsg) > 0 {
		fmt.Println(logMsg)
	} else if len(getPreviousProcessingState().msgLine) == 0 {
		// This is the first message in the log
		fmt.Println(line)
	}

	// Message processing machine returns non PG messages and related error (ex. CSV parsing) one
	// step behind current message. Storing this message to avoid loosing them.
	getPreviousProcessingState().storeState(line)
}
