package main

import (
	"dymium.com/dymium/log"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strings"
	"syscall"
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

	startTime := time.Now().UTC().Format("2006-01-02 15:04:05.193 UTC")
	EnvData.ComponentName = componentFlag
	EnvData.SourceName = sourceFlag
	log.Init(EnvData.ComponentName)

	tenant, ok := os.LookupEnv("CUSTOMER")
	if !ok || !(len(tenant) > 0) {
		_, _ = fmt.Fprintf(os.Stderr, "Tenant name is not defined.")
		panic("Tenant name is not defined.")
	}
	EnvData.Tenant = tenant

	StrLogCollector("INFO", fmt.Sprintf("%s Start collecting log messages from %s", startTime, pipeFlag))

	// Open the named pipe for reading
	pipe, err := os.OpenFile(pipeFlag, os.O_RDONLY, 0666)
	if err != nil {
		fmt.Println("Error opening named pipe:", err)
		return
	}
	defer pipe.Close()

	// Set up signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	// Set up channels for communication between goroutines
	dataChan := make(chan []byte, 1024)
	doneChan := make(chan bool)

	// Goroutine for reading from named pipe
	go func() {
		for {
			select {
			case <-doneChan:
				fmt.Println("Done.")
				return
			default:
				buffer := make([]byte, 4096)
				n, err := pipe.Read(buffer)
				if err != nil {
					if err != io.EOF {
						fmt.Println("Error reading from named pipe:", err)
						return
					}
					// keep waiting for new input
					//return
				}
				dataChan <- buffer[:n]
			}
		}
	}()

	// Goroutine for processing the data
	go func() {
		for data := range dataChan {
			// Placeholder for processing the data
			processData(data)
		}
	}()

	// Wait for a signal to exit
	sig := <-sigChan

	StrLogCollector("INFO", fmt.Sprintf("Log collector exiting.(signal: %s)", sig.String()))
	fmt.Println("Exiting.")
	close(doneChan)

}

type LineBuilder struct {
	sbBuff       *strings.Builder
	insideQoutes bool
	prevBSlash   bool
}

var lineBuilder = LineBuilder{new(strings.Builder), false, false}

func processData(data []byte) {

	if len(data) > 0 {
		for _, b := range data {
			if !lineBuilder.insideQoutes && b == '\n' {
				lineBuilder.sbBuff.WriteByte(b)
				ProcessMessage(lineBuilder.sbBuff.String())
				lineBuilder.sbBuff.Reset()
				lineBuilder.insideQoutes = false
			} else {
				if b == '"' && !lineBuilder.prevBSlash {
					lineBuilder.insideQoutes = !lineBuilder.insideQoutes
				}
				lineBuilder.sbBuff.WriteByte(b)
				if b == '\\' {
					lineBuilder.prevBSlash = !lineBuilder.prevBSlash
				} else {
					lineBuilder.prevBSlash = false
				}
			}
		}
	}
}
