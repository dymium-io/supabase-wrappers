package main

import (
	"github.com/aws/aws-lambda-go/lambda"

	"os"
	
	"database/sql"
	_ "github.com/lib/pq"

	"github.com/aclements/go-gg/generic/slice"
	"strconv"
	// "strings"
	"fmt"
	
	"DbSync/types"
)

var db *sql.DB

func LambdaHandler(c types.Request) (interface{}, error) {

	if c.Action == types.A_Update || c.Action == types.A_Delete {
		type empty struct{}
		return &empty{} , nil
	}

	if db == nil {
		if err := openDb(); err != nil {
			return nil, err
		}
	}

	database, err := getDbInfo(db, c.Customer, c.Datascope)
	if err != nil {
		return nil ,err
	}

	return &types.CustomerData{ Connections: []types.Connection{}, Datascopes: *database }, nil
}

func getDbInfo(db *sql.DB, infoSchema string, infoDatascope *string) (*[]types.Datascope,error) {
	infoDatascope_ := ""; if infoDatascope != nil { infoDatascope_ = fmt.Sprintf("d.name = %s AND ",*infoDatascope) }
	rows, err := db.Query(fmt.Sprintf(`SELECT d.name, c.id,
                                      t.schem, t.tabl, t.col,
                                      t.typ, t.is_nullable,
                                      t.semantics, t.action
                               FROM %s.tables t, %s.datascopes d, %s.connections c
                               WHERE %s t.datascope_id = d.id AND t.connection_id = c.id
                               ORDER BY d.name, t.schem, t.tabl, t.connection_id, t."position"`,
		infoSchema, infoSchema, infoSchema, infoDatascope_))
	if err != nil {
		return nil,err
	}
	defer rows.Close()
	if err != nil {
		return nil,err
	}
	defer rows.Close()

	datascopes := []types.Datascope{}
	var d *types.Datascope
	var s *types.Schema
	var t *types.Table
	for rows.Next() {
		var dscope, con, schem, tblName string
		var col types.Column
		err = rows.Scan(&dscope, &con, &schem, &tblName,
			&col.Name, &col.Typ, &col.IsNullable, &col.Semantics, &col.Action)
		if err != nil {
			return nil,err
		}
		if d == nil || dscope != d.Name {
			datascopes = append(datascopes, types.Datascope{
				Name: dscope,
				Connections: []string{ con },
				Schemas: []types.Schema{
					{
						Name: schem,
						Tables: []types.Table{
							{
								Name: tblName,
								Connection: con,
								Columns: []types.Column{ col },
							},
						},
					},
				},
			})
			d = &datascopes[len(datascopes)-1]
			s = &d.Schemas[0]
			t = &s.Tables[0]
		} else if schem != s.Name {
			d.Schemas = append(d.Schemas, types.Schema{
				Name: schem,
				Tables: []types.Table{
					{
						Name: tblName,
						Connection: con,
						Columns: []types.Column{ col },
					},
				},
			})
			d.Connections = append(d.Connections, con)
			s = &d.Schemas[len(d.Schemas)-1]
			t = &s.Tables[0]
		} else if tblName != t.Name || con != t.Connection {
			s.Tables = append(s.Tables, types.Table{
				Name: tblName,
				Connection: con,
				Columns: []types.Column{ col },
			})
			d.Connections = append(d.Connections, con)			
			t = &s.Tables[len(s.Tables)-1]
		} else {
			t.Columns = append(t.Columns, col)
		}
	}

	for k := range datascopes {
		d := &datascopes[k]
		slice.Sort(d.Connections)
		d.Connections = slice.Nub(d.Connections).([]string)
	}

	return &datascopes, nil
}

func openDb () error {
	var host_, port_, user_, password_, dbname_, sslmode_ string
	var err error
	getEnv := func (e string) (string,error) {
		if s := os.Getenv(e); s != "" {
			return s, nil
		} else {
			return "",fmt.Errorf("Lambda misconfiguration: [%s] is not defined",e)
		}
	}
	if host_,err = getEnv("DATABASE_HOST"); err != nil { return err }
	if port_,err = getEnv("DATABASE_PORT"); err != nil { return err }
	if dbname_,err = getEnv("DATABASE_DB"); err != nil { return err }
	if user_,err = getEnv("DATABASE_USER"); err != nil { return err }
	if password_,err = getEnv("DATABASE_PASSWORD"); err != nil { return err }
	if sslmode_,err = getEnv("DATABASE_TLS"); err != nil { return err }
	if p, err := strconv.Atoi(port_); err != nil || p < 1 || p > 65535 {
		return fmt.Errorf("Lambda misconfiguration: DATABASE_PORT value %s is wrong",port_)
	}
	psqlconn := fmt.Sprintf("host=%s port=%s dbname=%s user=%s password='%s' sslmode=%s",
		host_, port_, dbname_, user_, password_, sslmode_)
	db, err = sql.Open("postgres", psqlconn)
	if err != nil {
		return err
	}
	return nil
}

func main() {
        lambda.Start(LambdaHandler)
}
