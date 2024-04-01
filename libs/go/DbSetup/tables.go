package DbSetup

import (
	"crypto/sha256"
	"fmt"
	"strings"

	"dymium.io/DbSetup/types"
)

type tableViews struct {
	schemas []string
	tables  []tableView
}

type tableView struct {
	remoteSchema    string
	remoteName      string
	hiddenTableName string
	connection      string
	views           []viewT
	columns         []types.Column
}

type viewT struct {
	schema string
	name   string
}

func createTableViews(datascope types.Scope, connections map[string]types.Connection) tableViews {

	// stage 1: escape schema names and avoid conflicts within connections
	//          E.g., if the orifginal names are the same after capitalization (such as "schema" and "Schema"),
	//          both of them should be "Literary escaped", otherwise we will have conflict even after
	//          prepending these names with the names of the server
	// As the result, we will get the list of boolean flags, ordered exactly as the database.Schemas,
	//          that has 'true' value for each conflicting value
	conflicted := make([]bool, len(datascope.Schemas))
	for _, indices := range classify(datascope.Schemas, func(k int, c *types.Schema) (string, int) {
		return c.Tables[0].Connection, k
	}) {
		b := findDuplicates(indices, func(k int) string {
			return strings.ToLower(datascope.Schemas[k].Name)
		})
		for k, index := range indices {
			conflicted[index] = b[k]
		}
	}

	// stage 2: flatten tables list from datascope.Schemas.
	//          For each table, resolve possible conflicts in field names.
	//          Then, for each table create a list of views: the long view, short view,
	//          and, possibly, "public".
	schemas := make([]string, 0)
	tblMap := make(map[string][]pair[int, int])

	tables := make([]tableView, 0)
	ind := 0
	for ks := range datascope.Schemas {
		s := &datascope.Schemas[ks]
		var sn string
		if conflicted[ks] {
			sn = LiteralEscape(s.Name)
		} else {
			sn = PostgresEscape(s.Name)
		}
		cname := combineWithConn(connections[s.Tables[0].Connection].Name, sn)
		schemas = append(schemas, cname)
		snames := []string{
			cname,
			sn,
		}
		switch connections[datascope.Connections[ks]].Database_type {
		case types.CT_SqlServer:
			if s.Name == "dbo" {
				snames = append(snames, "public")
			}
		}
		remoteSchema := s.Name
		switch connections[datascope.Connections[ks]].Database_type {
		case types.CT_MongoDB:
			remoteSchema = connections[datascope.Connections[ks]].Dbname
		}
		for kt := range s.Tables {
			t := &s.Tables[kt]
			// first, resolve duplicates in column names
			b := findDuplicates(t.Columns, func(c types.Column) string {
				return strings.ToLower(c.Name)
			})
			for kc := range t.Columns {
				if b[kc] {
					t.Columns[kc].Name = LiteralEscape(t.Columns[kc].Name)
				} else {
					t.Columns[kc].Name = PostgresEscape(t.Columns[kc].Name)
				}
			}
			// now, create views
			v := make([]viewT, len(snames))
			for ksn, sn := range snames {
				v[ksn] = viewT{
					schema: sn,
					name:   PostgresEscape(t.Name),
				}
				if ksn > 0 {
					if tblMap[sn] == nil {
						tblMap[sn] = make([]pair[int, int], 0)
					}
					tblMap[sn] = append(tblMap[sn], pair[int, int]{fst: ind, snd: ksn})
				}
			}
			tables = append(tables, tableView{
				hiddenTableName: hiddenTableName(t.Connection, s.Name, t.Name),
				remoteSchema:    remoteSchema,
				remoteName:      t.Name,
				connection:      t.Connection,
				views:           v,
				columns:         t.Columns,
			})
			ind += 1
		}

	}

	// stage 3: resolve duplicates within short schemas
	for s, refs := range tblMap {
		if s != "public" {
			schemas = append(schemas, s)
		}
		b := findDuplicates(refs, func(r pair[int, int]) string {
			return strings.ToLower(tables[r.fst].views[r.snd].name)
		})
		for k, r := range refs {
			if b[k] {
				tables[r.fst].views[r.snd].name =
					combineWithConn(connections[tables[r.fst].connection].Name, tables[r.fst].views[r.snd].name)
			}
		}
	}

	return tableViews{
		schemas: schemas,
		tables:  tables,
	}
}

func combineWithConn(con, sch string) string {
	if sch[0] == '"' {
		return `"` + con + `_` + sch[1:]
	} else {
		return con + `_` + sch
	}
}

func hiddenTableName(dn, sn, tn string) string {
	return fmt.Sprintf("_dymium._%x_", sha256.Sum224([]byte(dn+sn+tn)))
}

func classify[T any, K comparable, V any](tss []T, f func(k int, t *T) (K, V)) map[K][]V {
	r := make(map[K][]V)
	for k := range tss {
		kss, v := f(k, &tss[k])
		if r[kss] == nil {
			r[kss] = make([]V, 0)
		}
		r[kss] = append(r[kss], v)
	}
	return r
}

func findDuplicates[T any](items []T, getName func(T) string) []bool {
	nameCount := make(map[string]int)
	extracted := make([]string, len(items))

	for k, item := range items {
		name := getName(item)
		extracted[k] = name
		nameCount[extracted[k]]++
	}

	result := make([]bool, len(items))
	for k := range result {
		result[k] = nameCount[extracted[k]] > 1
	}

	return result
}

type pair[K1 any, K2 any] struct {
	fst K1
	snd K2
}
