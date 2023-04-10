package logsparser

import "fmt"

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

// ProcessMessage Message processor. Called on log events to process log message. Collects all
// multiline log strings, parse log message if it CSV PostgreSQL message and forward it
// to log collector (ex.: local_env->stdout, aws_env->kinesis).
func (processor *PgLogProcessor) ProcessMessage(line string) {
	var logMsg string

	pgMsg, err := processor.Parser.ProcessPgLogMessage(line)
	if err == nil {
		if pgMsg != nil {
			// Finished msg processing.
			// This is Postgres message - add meta data and forward to collector
			logMsg, err = pgMsg.JsonString()
			if err != nil {
				// TODO set default/error metadata for dlog
				logMsg = line
			} else {
				// TODO create metadata for the log based on env vars and pgMsg struct
			}
		}
	} else {
		// TODO set default/error metadata for dlog
		logMsg = line
	}
	// TODO based on metadata and logSeverity->logFunc map forward message to log collector
	fmt.Println(logMsg)
}
