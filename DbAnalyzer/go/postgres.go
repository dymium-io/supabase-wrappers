package main

import (

	"database/sql"
	_ "github.com/lib/pq"

	"strings"
	"fmt"
	
	"DbAnalyzer/types"
)


func getPostgresInfo(c types.Connection) (interface{},error) {
	
	psqlconn := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=%s",
		c.Address, c.Port, c.User, c.Password, c.Database,
		func () string { if c.Tls { return "require" } else {return "disable" } }())
	
	db, err := sql.Open("postgres", psqlconn)
	if err != nil {
		return nil,err
	}
	defer db.Close()
	if err := db.Ping(); err != nil {
		return nil,err
	}
	if c.TestOnly {
		return struct{}{}, nil
	}

	
	rows, err := db.Query(`SELECT c.table_schema, c.table_name, c.ordinal_position, c.column_name,
                                      c.data_type,
                                      c.character_maximum_length,
                                      c.numeric_precision, c.numeric_scale,
                                      e.data_type,
                                      e.character_maximum_length,
                                      e.numeric_precision, e.numeric_scale,
                                      c.is_nullable, c.column_default
                               FROM information_schema.columns c
                               LEFT JOIN information_schema.element_types e
                               ON ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier)
                                  = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier))
                               ORDER BY c.table_schema, c.table_name, c.ordinal_position`)
	if err != nil {
		return nil,err
	}
	defer rows.Close()

	database := types.Database{
		Name: c.Database,
		Schemas: []types.Schema{},
		Refs: []types.Arc{},
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
		var eTyp *string
		var eCharMaxLen, ePrecision, eScale *int
		err = rows.Scan(&schema, &tblName, &pos, &cName,
			&cTyp, &cCharMaxLen, &cPrecision, &cScale,
			&eTyp, &eCharMaxLen, &ePrecision, &eScale,
			&cIsNullable, &dflt)
		if err != nil {
			return nil,err
		}
		isNullable := false; if cIsNullable == "YES" { isNullable = true }
		var t string
		switch cTyp {
		case "numeric":
			if cPrecision != nil {
				if cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)",*cPrecision,*cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)",*cPrecision)
				}
			} else { t = "numeric" }
		case "character varying":
			if cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)",*cCharMaxLen)
			} else {
				t = "varchar"
			}
		case "character":
			if cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)",*cCharMaxLen)
			} else {
				t = "bpchar"
			}
		case "ARRAY":
			switch *eTyp {
			case "numeric":
				if ePrecision != nil {
					if cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)[]",*ePrecision,*eScale)
					} else {
						t = fmt.Sprintf("numeric(%d)[]",*ePrecision)
					}
				} else { t = "numeric[]" }
			case "character varying":
				if eCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)[]",*eCharMaxLen)
				} else {
					t = "varchar[]"
				}
			case "character":
				if eCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)[]",*eCharMaxLen)
				} else {
					t = "character[]"
				}
			default:
				t = *eTyp+"[]"
			}
		default:
			t = cTyp
		}
		c := types.Column{
			Name: cName,
			Position: pos,
			Typ: t,
			IsNullable: isNullable,
			Default: dflt,
			Reference: nil,
			Semantics: nil,
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
						Columns: []types.Column{ c },
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
					Columns: []types.Column{ c },
				})
			curTbl += 1
		} else {
			database.Schemas[curSchema].Tables[curTbl].Columns =
				append(database.Schemas[curSchema].Tables[curTbl].Columns, c)
		}
	}

	if err = resolvePostgresRefs(db, &database); err != nil {
		return nil,err
	}
	
	return &database, nil
}

func resolvePostgresRefs(db *sql.DB, database *types.Database) error {
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
								database.Refs = append(database.Refs,
									types.Arc{
										From_schema: schema,
										From_table: tblName,
										From_column: columnName,
										To_schema: fSchema,
										To_table: fTblName,
										To_column: fColumnName,
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
