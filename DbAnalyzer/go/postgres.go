package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"strings"

	"DbAnalyzer/types"
)

func connectPostgres(c *types.ConnectionParams) (*sql.DB, error) {
	psqlconn := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=%s",
		c.Address, c.Port, c.User, c.Password, c.Database,
		func() string {
			if c.Tls {
				return "require"
			} else {
				return "disable"
			}
		}())

	db, err := sql.Open("postgres", psqlconn)
	if err != nil {
		return nil, err
	}
	if err := db.Ping(); err != nil {
		return nil, err
	}
	return db, nil
}

func getPostgresInfo(dbName string, db *sql.DB) (*types.DatabaseInfo, error) {

	rows, err := db.Query(`SELECT table_schema, table_name
                               FROM information_schema.tables
                               ORDER BY table_schema, table_name`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	database := types.DatabaseInfo{
		DbName:    dbName,
		Schemas: []types.Schema{},
	}
	curSchema := -1
	isSystem := false
	for rows.Next() {
		var schema, tblName string
		err = rows.Scan(&schema, &tblName)
		if err != nil {
			return nil, err
		}
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			if strings.HasPrefix(schema, "pg_") || schema == "information_schema" {
				isSystem = true
			} else {
				isSystem = false
			}
			database.Schemas = append(database.Schemas, types.Schema{
				Name:     schema,
				IsSystem: isSystem,
				Tables: []types.Table{
					{
						Name:     tblName,
						IsSystem: isSystem,
					},
				},
			})
			curSchema += 1
		} else {
			database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
				types.Table{
					Name:     tblName,
					IsSystem: isSystem,
				})
		}
	}

	return &database, nil
}


func getPostgresTblInfo(dbName string, tip *types.TableInfoParams, db *sql.DB) (*types.TableInfo, error) {
	
	rows, err := db.Query(`SELECT c.ordinal_position, c.column_name,
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
                               WHERE c.table_schema = ? and c.table_name = ?
                               ORDER BY c.ordinal_position`, tip.Schema, tip.Table)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	ti := types.TableInfo{
		DbName:    dbName,
		Schema:    tip.Schema,
		TblName:   tip.Table,
	}
	for rows.Next() {
		var cName string
		var pos int
		var cIsNullable string
		var dflt *string
		var cTyp string
		var cCharMaxLen, cPrecision, cScale *int
		var eTyp *string
		var eCharMaxLen, ePrecision, eScale *int
		err = rows.Scan(&pos, &cName,
			&cTyp, &cCharMaxLen, &cPrecision, &cScale,
			&eTyp, &eCharMaxLen, &ePrecision, &eScale,
			&cIsNullable, &dflt)
		if err != nil {
			return nil, err
		}
		isNullable := false
		if cIsNullable == "YES" {
			isNullable = true
		}
		var possibleActions *[]types.DataHandling
		var t string
		switch cTyp {
		case "numeric":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
			if cPrecision != nil {
				if cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *cPrecision, *cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *cPrecision)
				}
			} else {
				t = "numeric"
			}
		case "character varying":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
			if cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *cCharMaxLen)
			} else {
				t = "varchar"
			}
		case "character":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
			if cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *cCharMaxLen)
			} else {
				t = "bpchar"
			}
		case "ARRAY":
			switch *eTyp {
			case "numeric":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				if ePrecision != nil {
					if cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)[]", *ePrecision, *eScale)
					} else {
						t = fmt.Sprintf("numeric(%d)[]", *ePrecision)
					}
				} else {
					t = "numeric[]"
				}
			case "character varying":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				if eCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)[]", *eCharMaxLen)
				} else {
					t = "varchar[]"
				}
			case "character":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				if eCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)[]", *eCharMaxLen)
				} else {
					t = "character[]"
				}
			default:
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = *eTyp + "[]"
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
			PossibleActions: *possibleActions,
		}
		ti.Columns = append(ti.Columns, c)
	}

	if err = resolvePostgresRefs(db, tip, &ti); err != nil {
		return nil, err
	}

	return &ti, nil
}

func resolvePostgresRefs(db *sql.DB, tip *types.TableInfoParams, ti *types.TableInfo) error {
	rows, err := db.Query(`
           SELECT
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
	   WHERE tc.table_schema = ? and tc.table_name = ? and tc.constraint_type = 'FOREIGN KEY'`,
		tip.Schema, tip.Table)
	if err != nil {
		return err
	}
	defer rows.Close()

	for rows.Next() {
		var constraintName, columnName, fSchema, fTblName, fColumnName string
		err := rows.Scan(&constraintName, &columnName, &fSchema, &fTblName, &fColumnName)
		if err != nil {
			return err
		}
		for k := range ti.Columns {
			if ti.Columns[k].Name == columnName {
				ti.Columns[k].Reference = &types.Reference{
					Schema: fSchema,
					Table:  fTblName,
					Column: fColumnName,
				}
			}
		}
	}

	return nil
}
