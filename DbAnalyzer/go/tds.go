package main

import (
	"database/sql"
	//_ "github.com/thda/tds"
	_ "github.com/denisenkom/go-mssqldb"
	"fmt"
	"net/url"

	"DbAnalyzer/types"
)

func connectTds(c *types.ConnectionParams) (*sql.DB, error) {
	query := url.Values{}
	query.Add("database",c.Database)
	if c.Tls {
		query.Add("encrypt","true")
	} else {
		query.Add("encrypt","disable")
	}
	u := &url.URL{
		Scheme:   "sqlserver",
		User:     url.UserPassword(c.User, c.Password),
		Host:     fmt.Sprintf("%s:%d", c.Address, c.Port),
		// Path:  instance, // if connecting to an instance instead of a port
		RawQuery: query.Encode(),
	}
	tdsconn := u.String()

	fmt.Println("tdsconn:",tdsconn)
	db, err := sql.Open("sqlserver", tdsconn)
	if err != nil {
		return nil, err
	}
	if err := db.Ping(); err != nil {
		return nil, err
	}
	return db,nil
}

func getTdsInfo(dbName string, db *sql.DB) (*types.DatabaseInfo, error) {
	return nil, fmt.Errorf("getTdsInfo not implemented")
}

func getTdsTableInfo(dbName string, db *sql.DB) (interface{}, error) {
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

	database := types.DatabaseInfo{
		DbName:    dbName,
		Schemas: []types.Schema{},
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
				t = "bpchar"
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
		_ = c
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			switch schema {
			case "information_schema", "sys", "performance_schema":
				isSystem = true
			default:
				isSystem = false
			}
			database.Schemas = append(database.Schemas, types.Schema{
				Name:     schema,
				IsSystem: isSystem,
				Tables: []types.Table{
					{
						Name:     tblName,
						IsSystem: isSystem || isSysTable(tblName),
						// Columns:  []types.Column{c},
					},
				},
			})
			curSchema += 1
			curTbl = 0
		} else if tblName != database.Schemas[curSchema].Tables[curTbl].Name {
			database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
				types.Table{
					Name:     tblName,
					IsSystem: isSystem || isSysTable(tblName),
					// Columns:  []types.Column{c},
				})
			curTbl += 1
		} else {
			/*
			database.Schemas[curSchema].Tables[curTbl].Columns =
				append(database.Schemas[curSchema].Tables[curTbl].Columns, c)
			*/
		}
	}

	/*
	if err = resolveTdsRefs(db, &database); err != nil {
		return nil, err
	}
	*/

	return &database, nil
}

/*
func resolveTdsRefs(db *sql.DB, database *types.Database) error {
	rows, err := db.Query(`
	  SELECT sch.name AS [table_schema],
              obj.name AS [constraint_name],
	      tab1.name AS [table_name],
	      col1.name AS [column_name],
              sch2.name AS [foreign_table_schema],
	      tab2.name AS [foreign_table_name],
	      col2.name AS [foreign_column_name]
	  FROM sys.foreign_key_columns fkc
	  INNER JOIN sys.objects obj
	      ON obj.object_id = fkc.constraint_object_id
	  INNER JOIN sys.tables tab1
	      ON tab1.object_id = fkc.parent_object_id
	  INNER JOIN sys.schemas sch
	      ON tab1.schema_id = sch.schema_id
	  INNER JOIN sys.columns col1
	      ON col1.column_id = parent_column_id AND col1.object_id = tab1.object_id
	  INNER JOIN sys.tables tab2
	      ON tab2.object_id = fkc.referenced_object_id
	  INNER JOIN sys.schemas sch2
	      ON tab2.schema_id = sch2.schema_id
	  INNER JOIN sys.columns col2
	      ON col2.column_id = referenced_column_id AND col2.object_id = tab2.object_id`)
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
*/

func isSysTable(tblName string) bool {
	return tblName == "MSreplication_options" ||
		tblName == "spt_fallback_db" ||
		tblName == "spt_fallback_dev" ||
		tblName == "spt_fallback_usg" ||
		tblName == "spt_monitor" ||
		tblName == "spt_values"
}
