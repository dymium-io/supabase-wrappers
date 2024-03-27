package DbSetup

import (
	"regexp"
	"strings"
)

func esc(str string) string {
	return ParamEscape(str)
}

var PostgresEscape func(tok string) string

func LiteralEscape(tok string) string {
	return `"` + strings.ReplaceAll(tok, `"`, `""`) + `"`
}

func ParamEscape(tok string) string {
	return strings.ReplaceAll(tok, `'`, `''`)
}

func findDuplicates[T any](items []T, getName func(T) string) []bool {
	nameCount := make(map[string]int)
	lowered := make([]string, len(items))

	for k, item := range items {
		name := getName(item)
		lowered[k] = strings.ToLower(name)
		nameCount[lowered[k]]++
	}

	result := make([]bool, len(items))
	for k := range result {
		result[k] = nameCount[lowered[k]] > 1
	}

	return result
}

func init() {
	kw := regexp.MustCompile("(?i)^(ALL|ANALYSE|ANALYZE|AND|ANY|ARRAY|AS|ASC|ASYMMETRIC|AUTHORIZATION|BINARY|BOTH|CASE|CAST|CHECK|COLLATE|COLLATION|COLUMN|CONCURRENTLY|CONSTRAINT|CREATE|CROSS|CURRENT_CATALOG|CURRENT_DATE|CURRENT_ROLE|CURRENT_SCHEMA|CURRENT_TIME|CURRENT_TIMESTAMP|CURRENT_USER|DEFAULT|DEFERRABLE|DESC|DISTINCT|DO|ELSE|END|EXCEPT|FALSE|FETCH|FOR|FOREIGN|FREEZE|FROM|FULL|GRANT|GROUP|HAVING|ILIKE|IN|INITIALLY|INNER|INTERSECT|INTO|IS|ISNULL|JOIN|LATERAL|LEADING|LEFT|LIKE|LIMIT|LOCALTIME|LOCALTIMESTAMP|NATURAL|NOT|NOTNULL|NULL|OFFSET|ON|ONLY|OR|ORDER|OUTER|OVERLAPS|PLACING|PRIMARY|REFERENCES|RETURNING|RIGHT|SELECT|SESSION_USER|SIMILAR|SOME|SYMMETRIC|TABLE|TABLESAMPLE|THEN|TO|TRAILING|TRUE|UNION|UNIQUE|USER|USING|VARIADIC|VERBOSE|WHEN|WHERE|WINDOW|WITH)$")
	invalid := regexp.MustCompile(`(^\d)|([^a-zA-Z0-9_])`)

	PostgresEscape = func(tok string) string {
		if invalid.MatchString(tok) {
			return `"` + strings.ReplaceAll(tok, `"`, `""`) + `"`
		} else if kw.MatchString(tok) {
			return `"` + tok + `"`
		}
		return tok
	}
}
