package main

import (
	"dymium.com/dymium/log"
	"fmt"
	"github.com/go-cmd/cmd"
	"logsupervisor/logsparser"
	"os"
	"time"
)

func main() {

	argsWithProg := os.Args
	nArgs, startTime := len(argsWithProg), time.Now().Format("2023-04-04 03:20:24.193 UTC")
	var cmd string
	var args []string
	componentName := "logsupervisor"
	log.Init(componentName)
	commandLine := ""

	if 2 <= nArgs {
		commandLine = fmt.Sprintf(
			"LogSupervisor: Starting %s with args:%s",
			argsWithProg[1],
			argsWithProg[2:],
			startTime,
		)
		cmd = argsWithProg[1]
		args = argsWithProg[2:]
	} else {
		commandLine = fmt.Sprintf(
			"Log-Supervisor: Starting %s",
			argsWithProg,
			startTime,
		)
		cmd = argsWithProg[1]
	}

	logsparser.StrLogCollector("INFO", commandLine)

	runner(processLine, cmd, args...)
	logsparser.StrLogCollector("INFO", fmt.Sprintf("%s exiting.", componentName))
}

var LogParser *logsparser.PgLogProcessor

func processLine(line string) {
	LogParser.ProcessMessage(line, false)
}

func flushMsgBuffer() {
	LogParser.ProcessMessage("", true)
}

func runner(lineproc func(line string), command string, args ...string) {
	LogParser = logsparser.NewPgLogProcessor()

	// Disable output buffering, enable streaming
	cmdOptions := cmd.Options{
		Buffered:  false,
		Streaming: true,
	}

	envCmd := cmd.NewCmdOptions(cmdOptions, command, args...)

	// Collect STDOUT lines streaming from Cmd
	doneChan := make(chan struct{})
	go func() {
		defer close(doneChan)
		// Done when both channels have been closed
		// https://dave.cheney.net/2013/04/30/curious-channels
		for envCmd.Stdout != nil || envCmd.Stderr != nil {
			select {
			case line, open := <-envCmd.Stdout:
				if !open {
					envCmd.Stdout = nil
					continue
				}
				if len(line) != 0 {
					lineproc(line)
				}
			case line, open := <-envCmd.Stderr:
				if !open {
					envCmd.Stderr = nil
					continue
				}
				log.Errorf(line)
			}
		}
	}()

	// Run and wait for Cmd to return, discard Status
	<-envCmd.Start()

	// Wait for goroutine to print everything
	<-doneChan
	flushMsgBuffer()
}
