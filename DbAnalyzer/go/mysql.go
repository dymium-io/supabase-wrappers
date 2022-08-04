package main

import (
	"database/sql"
	_ "github.com/go-sql-driver/mysql"

	"fmt"

	"DbAnalyzer/types"
)

func getMysqlInfo(c types.Connection) (interface{}, error) {

	mysqlconn := fmt.Sprintf("%s:%s@tcp(%s:%d)/?tls=%s",
		c.User, c.Password, c.Address, c.Port,
		func() string {
			if c.Tls {
				return "true"
			} else {
				return "false"
			}
		}())

	db, err := sql.Open("mysql", mysqlconn)
	if err != nil {
		return nil, err
	}
	defer db.Close()
	if err := db.Ping(); err != nil {
		return nil, err
	}
	if c.TestOnly {
		return struct{}{}, nil
	}

	rows, err := db.Query(`SELECT table_schema, table_name, ordinal_position, column_name,
                                      data_type,
                                      character_maximum_length,
                                      numeric_precision, numeric_scale,
                                      is_nullable, column_default
                               FROM information_schema.columns
                               ORDER BY table_schema, table_name, ordinal_position`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	database := types.Database{
		Name:    c.Database,
		Schemas: []types.Schema{},
		Refs:    []types.Arc{},
	}
	curSchema := -1
	curTbl := -1
	isSystem := false
	for rows.Next() {
		var schema, tblName, cName string
		var pos int
		var cIsNullable string
		var dflt *string
		var cTyp string
		var cCharMaxLen, cPrecision, cScale *int
		err = rows.Scan(&schema, &tblName, &pos, &cName,
			&cTyp, &cCharMaxLen, &cPrecision, &cScale,
			&cIsNullable, &dflt)
		if err != nil {
			return nil, err
		}
		isNullable := false
		if cIsNullable == "YES" {
			isNullable = true
		}
		var t string
		switch cTyp {
		case "decimal":
			if cPrecision != nil {
				if cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *cPrecision, *cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *cPrecision)
				}
			} else {
				t = "numeric"
			}
		case "varchar":
			if cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *cCharMaxLen)
			} else {
				t = "varchar"
			}
		case "char":
			if cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *cCharMaxLen)
			} else {
				t = "dpchar"
			}
		default:
			t = cTyp
		}
		c := types.Column{
			Name:       cName,
			Position:   pos,
			Typ:        t,
			IsNullable: isNullable,
			Default:    dflt,
			Reference:  nil,
			Semantics:  nil,
		}
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			switch schema {
			case "information_schema", "mysql", "sys", "performance_schema": isSystem = true
			default: isSystem = false
			}
			database.Schemas = append(database.Schemas, types.Schema{
				Name:     schema,
				IsSystem: isSystem,
				Tables: []types.Table{
					{
						Name:     tblName,
						IsSystem: isSystem,
						Columns:  []types.Column{c},
					},
				},
			})
			curSchema += 1
			curTbl = 0
		} else if tblName != database.Schemas[curSchema].Tables[curTbl].Name {
			database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
				types.Table{
					Name:     tblName,
					IsSystem: isSystem,
					Columns:  []types.Column{c},
				})
			curTbl += 1
		} else {
			database.Schemas[curSchema].Tables[curTbl].Columns =
				append(database.Schemas[curSchema].Tables[curTbl].Columns, c)
		}
	}

	if err = resolveMysqlRefs(db, &database); err != nil {
		return nil, err
	}

	return &database, nil
}

func resolveMysqlRefs(db *sql.DB, database *types.Database) error {
	rows, err := db.Query(`
           SELECT
	       tc.table_schema, 
	       tc.constraint_name, 
	       kcu.table_name, 
	       kcu.column_name, 
	       kcu.referenced_table_schema AS foreign_table_schema,
	       kcu.referenced_table_name AS foreign_table_name,
	       kcu.referenced_column_name AS foreign_column_name
	   FROM 
	       information_schema.table_constraints AS tc 
	       JOIN information_schema.key_column_usage AS kcu
		 ON tc.constraint_name = kcu.constraint_name
		 AND tc.table_schema = kcu.table_schema
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
									Table:  fTblName,
									Column: fColumnName,
								}
								database.Refs = append(database.Refs,
									types.Arc{
										From_schema: schema,
										From_table:  tblName,
										From_column: columnName,
										To_schema:   fSchema,
										To_table:    fTblName,
										To_column:   fColumnName,
									})
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
