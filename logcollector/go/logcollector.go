package main

import (
	"bufio"
	"context"
	"dymium.com/dymium/log"
	"errors"
	"flag"
	"fmt"
	"logcollector/logsparser"
	"os"
	"time"
)

// Define the flag
var help = flag.Bool("help", false, "Show help")
var pipeFlag string
var sourceFlag string
var componentFlag string

func main() {
	// Bind the flag
	flag.StringVar(&pipeFlag,
		"pipename",
		"/tmp/logpipe",
		"Named pipe to monitor: ex. /tmp/logpipe")
	flag.StringVar(&sourceFlag,
		"sourcename",
		"logstream",
		"A string that will be added to log message as source name: ex. logstream, errstream.")
	flag.StringVar(&componentFlag,
		"componentname",
		"logcollector",
		"Component name that will be added to log messages.")

	// Parse the flag
	flag.Parse()

	// Usage Demo
	if *help {
		flag.Usage()
		os.Exit(0)
	}

	startTime := time.Now().Format("2023-04-04 03:20:24.193 UTC")
	logsparser.EnvData.ComponentName = componentFlag
	logsparser.EnvData.SourceName = sourceFlag
	log.Init(logsparser.EnvData.ComponentName)

	pipeName := pipeFlag
	pipe, err := os.OpenFile(pipeName, os.O_RDONLY, os.ModeNamedPipe)
	if err != nil {
		_, _ = fmt.Fprintf(os.Stderr, "Open named pipe file error: %s", err)
		panic("Open named pipe file error")
	}
	defer func() {
		_ = pipe.Close()
	}()

	tenant, ok := os.LookupEnv("CUSTOMER")
	if !ok || !(len(tenant) > 0) {
		_, _ = fmt.Fprintf(os.Stderr, "Tenant name is not defined.")
		panic("Tenant name is not defined.")
	}
	logsparser.EnvData.Tenant = tenant

	logsparser.StrLogCollector("INFO", fmt.Sprintf("%s Start collecting log messages from %", startTime, pipeName))

	err = runner(pipe, processLine)

	errMessage := "no errors"
	if err != nil {
		errMessage = err.Error()
	}

	logsparser.StrLogCollector("INFO", fmt.Sprintf("%s exiting with %.", logsparser.EnvData.ComponentName, errMessage))

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
		ctx, cancel := context.WithTimeout(context.Background(), timeOut)
		defer cancel()
		go readLineWithTimeout(ctx, reader, dataStream, errStream)

		select {
		case <-ctx.Done():
			flushMsgBuffer()
		case msg := <-dataStream:
			lineprocessor(msg)
		case err = <-errStream:
			return err
		}
	}
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
