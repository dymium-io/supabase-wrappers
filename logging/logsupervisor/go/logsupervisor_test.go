package main

import (
	"fmt"
	"testing"
)

var lcounter uint64 = 0

// TODO add log parser and dlog ...
func countLines(line string) {
	fmt.Printf("read:%d:%s\n", lcounter, line)
	lcounter++

}

func Test_runner(t *testing.T) {
	type args struct {
		lineproc func(line string)
		command  string
		args     []string
	}
	tests := []struct {
		name string
		args args
		//want - should it be int or bool?
	}{
		// TODO: Add test cases.
		{
			name: "Processed Number of line must match number of line sent by test app.",
			args: args{
				lineproc: countLines,
				command:  "../scripts/streamfile.py",
				args:     []string{"/Users/michael/example.log"},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			runner(tt.args.lineproc, tt.args.command, tt.args.args...)
		})
	}
}
