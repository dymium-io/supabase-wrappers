package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
)

func main() {
	//cmd := exec.Command("../scripts/streamfile.py", "/Users/michael/example.log")
	cmd := exec.Command("cat", "/Users/michael/example.log")

	var stdoutBuf, stderrBuf bytes.Buffer
	cmd.Stdout = io.MultiWriter(os.Stdout, &stdoutBuf)
	cmd.Stderr = io.MultiWriter(os.Stderr, &stderrBuf)

	err := cmd.Run()
	if err != nil {
		log.Fatalf("cmd.Run() failed with %s\n", err)
	}
	//outStr, errStr := string(stdoutBuf.Bytes()), string(stderrBuf.Bytes())
	outStr := string(stdoutBuf.Bytes())
	fmt.Printf("out: %s", outStr)
}
