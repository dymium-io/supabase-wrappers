package main

import (
	"fmt"
	"github.com/go-cmd/cmd"
	"logsupervisor/logsparser"
	"os"
	"time"
)

func main() {

	argsWithProg := os.Args
	nArgs, startTime := len(argsWithProg), time.Now().Format(time.RFC3339Nano)
	var cmd string
	var args []string

	if 2 <= nArgs {
		fmt.Fprintf(
			os.Stdout,
			"%s Log-Supervisor: Starting %s with args:%s\n",
			startTime,
			argsWithProg[1],
			argsWithProg[2:],
		)
		cmd = argsWithProg[1]
		args = argsWithProg[2:]
	} else {
		fmt.Fprintf(
			os.Stdout,
			"%s Log-Supervisor: Starting %s\n",
			startTime,
			argsWithProg,
		)
		cmd = argsWithProg[1]
	}

	runner(processLine, cmd, args...)
}

// TODO add log parser and dlog ...
// This is temporary code
var lc uint64 = 0
var LogParser *logsparser.PgLogProcessor

func processLine(line string) {
	LogParser.ProcessMessage(line)
}

func runner(lineproc func(line string), command string, args ...string) {
	LogParser = logsparser.NewPgLogProcessor()

	// Disable output buffering, enable streaming
	cmdOptions := cmd.Options{
		Buffered:  false,
		Streaming: true,
	}

	// Create Cmd with options
	envCmd := cmd.NewCmdOptions(cmdOptions, command, args...)

	// Print STDOUT and STDERR lines streaming from Cmd
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
				// TODO - should this message be sent to a log collector too?
				fmt.Fprintln(os.Stderr, line)
			}
		}
	}()

	// Run and wait for Cmd to return, discard Status
	<-envCmd.Start()

	// Wait for goroutine to print everything
	<-doneChan
}
