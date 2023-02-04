package main

import (
	"database/sql"
	_ "github.com/sijms/go-ora/v2"

	"fmt"
	"net/url"
	"strings"

	"DbAnalyzer/types"
)

func getOraInfo(c types.Connection) (interface{}, error) {

	query := url.Values{}
	if c.Tls {
		query.Add("SSL", "true")
	} else {
		query.Add("SSL", "false")
	}
	u := &url.URL{
		Scheme:   "oracle",
		User:     url.UserPassword(c.User, c.Password),
		Host:     fmt.Sprintf("%s:%d", c.Address, c.Port),
		Path:     strings.ToUpper(c.Database),
		RawQuery: query.Encode(),
	}
	oracleconn := u.String()

	db, err := sql.Open("oracle", oracleconn)
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

	rows, err := db.Query(`
			       SELECT c.OWNER,
                                      c.TABLE_NAME,
                                      c.COLUMN_ID,
                                      c.COLUMN_NAME,
                                      c.DATA_TYPE,
                                      c.CHAR_LENGTH,
                                      c.DATA_PRECISION,
                                      c.DATA_SCALE,
                                      c.NULLABLE,
                                      c.DEFAULT_LENGTH,
                                      c.DATA_DEFAULT
                               FROM ALL_TABLES t
                               INNER JOIN ALL_TAB_COLS c ON
                                 t.OWNER = c.OWNER AND t.TABLE_NAME = c.TABLE_NAME
                               WHERE t.TABLESPACE_NAME NOT IN ('SYSTEM', 'SYSAUX', 'UNDOTBS1', 'TEMP')
                               ORDER BY c.OWNER, c.TABLE_NAME, c.COLUMN_ID`)
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
		var pos *int
		var cIsNullable *string
		var cDflt *string
		var cTyp *string
		var cCharMaxLen, cPrecision, cScale *int
		var dLength *int
		err = rows.Scan(&schema, &tblName, &pos, &cName,
			&cTyp, &cCharMaxLen, &cPrecision, &cScale,
			&cIsNullable, &dLength, &cDflt)
		if err != nil {
			return nil, err
		}
		fmt.Println(schema, tblName, pos, cName, cTyp, cCharMaxLen, cPrecision, cScale, cIsNullable, dLength, cDflt)
		isNullable := true
		if cIsNullable != nil && *cIsNullable == "N" {
			isNullable = false
		}
		var t string
		if cTyp == nil {
			t = "undefined"
		} else {
			switch strings.ToLower(*cTyp) {
			case "number":
				if cPrecision != nil {
					if cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)", *cPrecision, *cScale)
					} else {
						t = fmt.Sprintf("numeric(%d)", *cPrecision)
					}
				} else {
					t = "numeric"
				}
			case "varchar2", "nvarchar2":
				if cCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)", *cCharMaxLen)
				} else {
					t = "varchar"
				}
			case "char", "nchar":
				if cCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)", *cCharMaxLen)
				} else {
					t = "bpchar"
				}
			default:
				t = *cTyp
			}
		}
		if pos == nil {
			p := 0
			pos = &p
		}
		var dflt *string
		if dLength == nil || *dLength == 0 || cDflt == nil {
			dflt = nil
		} else {
			d := (*cDflt)[:*dLength]
			dflt = &d
		}
		c := types.Column{
			Name:       cName,
			Position:   *pos,
			Typ:        t,
			IsNullable: isNullable,
			Default:    dflt,
			Reference:  nil,
			Semantics:  nil,
		}
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			switch schema {
			case "RDSADMIN":
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

	if err = resolveOraRefs(db, &database); err != nil {
		return nil, err
	}

	return &database, nil
}

func resolveOraRefs(db *sql.DB, database *types.Database) error {
	rows, err := db.Query(`
	   SELECT c.OWNER, a.CONSTRAINT_NAME, a.TABLE_NAME, a.COLUMN_NAME,
		  c.R_OWNER AS REF_OWNER, cpk.TABLE_NAME AS REF_TABLE,
                  c.R_CONSTRAINT_NAME
           FROM ALL_TABLES t
	   JOIN ALL_CONS_COLUMNS a ON t.OWNER = a.OWNER
               AND t.TABLE_NAME = a.TABLE_NAME
	   JOIN ALL_CONSTRAINTS c ON a.OWNER = c.OWNER
	       AND a.CONSTRAINT_NAME = c.CONSTRAINT_NAME
	   JOIN ALL_CONSTRAINTS cpk ON c.R_OWNER = cpk.OWNER
	       AND c.R_CONSTRAINT_NAME = cpk.CONSTRAINT_NAME
           WHERE t.TABLESPACE_NAME NOT IN ('SYSTEM', 'SYSAUX', 'UNDOTBS1', 'TEMP')
               AND c.CONSTRAINT_TYPE = 'R'`)
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
