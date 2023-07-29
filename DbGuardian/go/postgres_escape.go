package main

import (
	"initializer/types"
	"regexp"
	"strings"
)

var kw, valid *regexp.Regexp

func init() {
	kw = regexp.MustCompile("(?i)^(ALL|ANALYSE|ANALYZE|AND|ANY|ARRAY|AS|ASC|ASYMMETRIC|AUTHORIZATION|BINARY|BOTH|CASE|CAST|CHECK|COLLATE|COLLATION|COLUMN|CONCURRENTLY|CONSTRAINT|CREATE|CROSS|CURRENT_CATALOG|CURRENT_DATE|CURRENT_ROLE|CURRENT_SCHEMA|CURRENT_TIME|CURRENT_TIMESTAMP|CURRENT_USER|DEFAULT|DEFERRABLE|DESC|DISTINCT|DO|ELSE|END|EXCEPT|FALSE|FETCH|FOR|FOREIGN|FREEZE|FROM|FULL|GRANT|GROUP|HAVING|ILIKE|IN|INITIALLY|INNER|INTERSECT|INTO|IS|ISNULL|JOIN|LATERAL|LEADING|LEFT|LIKE|LIMIT|LOCALTIME|LOCALTIMESTAMP|NATURAL|NOT|NOTNULL|NULL|OFFSET|ON|ONLY|OR|ORDER|OUTER|OVERLAPS|PLACING|PRIMARY|REFERENCES|RETURNING|RIGHT|SELECT|SESSION_USER|SIMILAR|SOME|SYMMETRIC|TABLE|TABLESAMPLE|THEN|TO|TRAILING|TRUE|UNION|UNIQUE|USER|USING|VARIADIC|VERBOSE|WHEN|WHERE|WINDOW|WITH)$")
	valid = regexp.MustCompile("^[a-zA-Z_][a-zA-Z0-9_]*$")
}

func PostgresEscape(tok string) string {
	if valid.MatchString(tok) {
		if kw.MatchString(tok) || strings.Contains(tok, " ") {
			return `"` + tok + `"`
		} else {
			return tok
		}
	} else {
		return `"` + tok + `"`
	}
}

func SqlEscape(tok string, connectionType types.ConnectionType) string {
	if valid.MatchString(tok) {
		uptok := strings.ToLower(tok)
		if connectionType != "postgres" {
			uptok = strings.ToUpper(tok)
		}
		if kw.MatchString(tok) || uptok != tok || strings.Contains(tok, " ") {
			return `"` + tok + `"`
		} else {
			return tok
		}
	} else {
		return `"` + tok + `"`
	}
}
