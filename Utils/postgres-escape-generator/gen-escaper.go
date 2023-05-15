package main

import (
	"encoding/csv"
	"os"
	"strings"
	"text/template"
)

func main() {
	records,err := readCSV("postgres-reserved.csv")
	if err != nil {
		panic(err)
	}
	reserved := genRegexp(records)
	tmpl, err := template.ParseFiles("postgres_escape.go")
	if err != nil {
		panic(err)
	}
	err = tmpl.Execute(os.Stdout, strings.Join(reserved,"|"))
	if err != nil {
		panic(err)
	}
}

type record_t struct {
	key_word   string
	postgresql string
	sql_2016   string
	sql_2011   string
	sql_92     string
}

func genRegexp(records []record_t) []string {
	reserved := make([]string,0,len(records))
	for _, r := range records {
		if strings.HasPrefix(r.postgresql,"reserved") {
			reserved = append(reserved,r.key_word)
		}
	}
	return reserved
}

func readCSV(fn string) ([]record_t, error) {
	// Open the CSV file
	f, err := os.Open(fn)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	// Read in the CSV records
	r := csv.NewReader(f)
	records, err := r.ReadAll()
	if err != nil {
		return nil, err
	}

	pRecords := make([]record_t, 0, len(records)-1)
	for _, record := range records[1:] {
		pRecords = append(pRecords, record_t{
			key_word:   record[0],
			postgresql: record[1],
			sql_2016:   record[2],
			sql_2011:   record[3],
			sql_92:     record[4],
		})
	}
	
	return pRecords, nil
}
