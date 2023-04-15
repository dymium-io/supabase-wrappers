package main

import (
	"bufio"
	"dymium.com/dymium/log"
	"fmt"
	"logsupervisor/logsparser"
	"os"
	"strings"
	"time"
)

func main() {
	argsWithProg := os.Args
	nArgs, startTime := len(argsWithProg), time.Now().Format("2023-04-04 03:20:24.193 UTC")
	componentName := "logcollector"
	log.Init(componentName)

	if nArgs < 1 {
		fmt.Fprintf(os.Stderr, "Log named pipe is missing. Expected command: logcollector <NAMED_PIPE>\n")
		panic("Invalid command line")
	}
	pipeName := argsWithProg[1]
	pipe, err := os.OpenFile(pipeName, os.O_RDONLY, os.ModeNamedPipe)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Open named pipe file error: %s", err)
		panic("Open named pipe file error")
	}

	tenant, ok := os.LookupEnv("CUSTOMER")
	if !ok || !(len(tenant) > 0) {
		fmt.Fprintf(os.Stderr, "Tenant name is not defined.")
		// TODO should we do anything when the tenant is not present?
	}

	// TODO add tenant to the log
	logsparser.StrLogCollector("INFO", fmt.Sprintf("%s Start collecting log messages from %", startTime, pipeName))

	err = runner(pipe, processLine)

	errMessage := "no errors"
	if err != nil {
		errMessage = err.Error()
	}
	// TODO add tenant to the log
	logsparser.StrLogCollector("INFO", fmt.Sprintf("%s exiting with %.", componentName, errMessage))

}

var LogParser *logsparser.PgLogProcessor

func processLine(line string) {
	if len(line) > 0 {
		LogParser.ProcessMessage(line, false)
	}
}

func flushMsgBuffer() {
	LogParser.ProcessMessage("", true)
}

func LogFileEnded(msg string) bool {
	// TODO - add last message in PG log file. Should we simple compare or use regexp?
	// Without this we can loose the last meessage. But if we use it and encounter the template somewhere in the log
	// we can end the process prematurely.
	// For now set it to nil.
	var lastLogMsgTemplate *string = nil
	if lastLogMsgTemplate != nil && strings.Contains(msg, *lastLogMsgTemplate) {
		return true
	}
	return false
}

func runner(pipe *os.File, lineprocessor func(line string)) error {
	LogParser = logsparser.NewPgLogProcessor()
	reader := bufio.NewReader(pipe)

	var err error = nil
	for {
		line, err := reader.ReadBytes('\n')
		if err == nil {
			if len(line) > 0 {
				msg := string(line[:len(line)-1])
				lineprocessor(msg)
				if LogFileEnded(msg) {
					break
				}
			}
		}
	}

	// Done with the processing. Flush messages in the FSM buffer if any.
	flushMsgBuffer()
	return err
}
