package main

import (
	"regexp"
)

var kw,valid *regexp.Regexp

func init() {
	kw = regexp.MustCompile("(?i)^({{.}})$")
	valid = regexp.MustCompile("^[a-zA-Z_][a-zA-Z0-9_]*$")
}

func PostgresEscape(tok string) string {
	if valid.MatchString(tok) {
		if kw.MatchString(tok) {
			return `"`+tok+`"`
		} else {
			return tok
		}
	} else {
		return `"`+tok+`"`
	}
}
