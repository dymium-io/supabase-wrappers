package main

import (
	"github.com/aws/aws-lambda-go/lambda"
	
	"database/sql"
	_ "github.com/lib/pq"

	"strings"
	"fmt"
	
	"DbAnalyzer/types"
)


func LambdaHandler(c types.Connection) (*types.Database, error) {

	psqlconn := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=%s",
		c.Address, c.Port, c.User, c.Password, c.Database,
		func () string { if c.Tls { return "enable" } else {return "disable" } }())
	
	db, err := sql.Open("postgres", psqlconn)
	if err != nil {
		return nil,err
	}
	defer db.Close()

	database, err := getDbInfo(db, &c.Database)
	if err != nil {
		return nil,err
	}
	if err = resolveRefs(db, database); err != nil {
		return nil,err
	}

	return database, nil
}

func getDbInfo(db *sql.DB, dbName *string) (*types.Database,error) {
	rows, err := db.Query(`SELECT table_schema, table_name, ordinal_position, column_name, udt_name
                               FROM information_schema.columns
                               ORDER BY table_schema, table_name, ordinal_position`)
	if err != nil {
		return nil,err
	}
	defer rows.Close()

	database := types.Database{
		Name: *dbName,
		Schemas: []types.Schema{},
	}
	curSchema := -1
	curTbl := -1
	isSystem := false
	for rows.Next() {
		var schema, tblName, cName, typ string
		var pos int
		err = rows.Scan(&schema, &tblName, &pos, &cName, &typ)
		if err != nil {
			return nil,err
		}
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			if strings.HasPrefix(schema, "pg_") || schema == "information_schema" {
				isSystem = true
			} else {
				isSystem = false
			}
			database.Schemas = append(database.Schemas, types.Schema{
				Name: schema,
				IsSystem: isSystem,
				Tables: []types.Table{
					{
						Name: tblName,
						IsSystem: isSystem,
						Columns: []types.Column{
							{
								Name: cName,
								Position: pos,
								Typ: typ,
								Reference: nil,
								Semantics: nil,
							},
						},
					},
				},
			})
			curSchema += 1
			curTbl = 0
		} else if tblName != database.Schemas[curSchema].Tables[curTbl].Name {
			database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
				types.Table{
					Name: tblName,
					IsSystem: isSystem,
					Columns: []types.Column{
						{
							Name: cName,
							Position: pos,
							Typ: typ,
							Reference: nil,
							Semantics: nil,
						},
					},
				})
			curTbl += 1
		} else {
			database.Schemas[curSchema].Tables[curTbl].Columns =
				append(database.Schemas[curSchema].Tables[curTbl].Columns,
					types.Column{
						Name: cName,
						Position: pos,
						Typ: typ,
						Reference: nil,
						Semantics: nil,
					})
		}
	}

	return &database, nil
}

func resolveRefs(db *sql.DB, database *types.Database) error {
	rows, err := db.Query(`
           SELECT
	       tc.table_schema, 
	       tc.constraint_name, 
	       tc.table_name, 
	       kcu.column_name, 
	       ccu.table_schema AS foreign_table_schema,
	       ccu.table_name AS foreign_table_name,
	       ccu.column_name AS foreign_column_name 
	   FROM 
	       information_schema.table_constraints AS tc 
	       JOIN information_schema.key_column_usage AS kcu
		 ON tc.constraint_name = kcu.constraint_name
		 AND tc.table_schema = kcu.table_schema
	       JOIN information_schema.constraint_column_usage AS ccu
		 ON ccu.constraint_name = tc.constraint_name
		 AND ccu.table_schema = tc.table_schema
	   WHERE tc.constraint_type = 'FOREIGN KEY'`)
	if err != nil {
		return err
	}
	defer rows.Close()

	for rows.Next() {
		var schema, constraintName, tblName, columnName, fSchema, fTblName, fColumnName string
		err := rows.Scan(&schema, &constraintName, &tblName, &columnName, &fSchema, &fTblName, &fColumnName)
		if err != nil {
			return err
		}
	Loop:
		for kSchema := range database.Schemas {
			s := &database.Schemas[kSchema]
			if s.Name == schema {
				for kTbl := range s.Tables {
					t := &s.Tables[kTbl]
					if t.Name == tblName {
						for kColumn := range t.Columns {
							c := &t.Columns[kColumn]
							if c.Name == columnName {
								c.Reference = &types.Reference{
									Schema: fSchema,
									Table: fTblName,
									Column: fColumnName,
								}
								break Loop
							}
						}
					}
				}
			}
		}
	}

	return nil
}


func main() {
        lambda.Start(LambdaHandler)
}
