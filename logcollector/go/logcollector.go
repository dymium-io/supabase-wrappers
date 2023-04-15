package main

import (
	"bufio"
	"context"
	"dymium.com/dymium/log"
	"errors"
	"fmt"
	"logsupervisor/logsparser"
	"os"
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
	defer pipe.Close()

	tenant, ok := os.LookupEnv("CUSTOMER")
	if !ok || !(len(tenant) > 0) {
		fmt.Fprintf(os.Stderr, "Tenant name is not defined.")
		panic("Tenant name is not defined.")
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

func runner(pipe *os.File, lineprocessor func(line string)) error {
	LogParser = logsparser.NewPgLogProcessor()
	reader := bufio.NewReader(pipe)
	timeOut := time.Duration(time.Second * 1) // hardcoded timout for reading log messages - 1 sec, TODO should it be a parameter?
	//creating  channels
	dataStream := make(chan string, 1)
	errStream := make(chan error)

	var err error = nil
	for {
		ctx, _ := context.WithTimeout(context.Background(), timeOut)
		go readLineWithTimeout(ctx, reader, dataStream, errStream)

		select {
		case <-ctx.Done():
			flushMsgBuffer()
		case msg := <-dataStream:
			lineprocessor(msg)
		case err = <-errStream:
			break
		}
	}

	// Done with the processing. Flush messages in the FSM buffer if any.
	flushMsgBuffer()
	return err
}

func readLineWithTimeout(ctx context.Context, reader *bufio.Reader, data chan string, er chan error) {
	line, err := reader.ReadBytes('\n')
	if err == nil {
		if len(line) > 0 {
			msg := string(line[:len(line)-1])
			data <- msg
		} else {
			er <- errors.New("error reading pipe")
		}
	}
}
