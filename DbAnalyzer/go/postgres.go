package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"strings"

	"DbAnalyzer/types"
)

type Postgres struct {
	db *sql.DB
}

func (da *Postgres) Ping() error {
	return da.db.Ping()
}

func (da *Postgres) Close() {
	da.db.Close()
}

func (da *Postgres) Connect(c *types.ConnectionParams) error {
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
		return err
	}
	if err := db.Ping(); err != nil {
		return err
	}

	da.db = db
	return nil
}

func (da *Postgres) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {

	rows, err :=
		da.db.Query(`SELECT table_schema, table_name
                             FROM information_schema.tables
                             ORDER BY table_schema, table_name`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	database := types.DatabaseInfoData{
		DbName:  dbName,
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

func (da *Postgres) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {

	rows, err :=
		da.db.Query(`SELECT c.ordinal_position, c.column_name,
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
                             WHERE c.table_schema = $1 and c.table_name = $2
                             ORDER BY c.ordinal_position`, tip.Schema, tip.Table)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	ti := types.TableInfoData{
		DbName:  dbName,
		Schema:  tip.Schema,
		TblName: tip.Table,
	}

	type data struct {
		cName                           string
		pos                             int
		isNullable                      bool
		dflt                            *string
		cTyp                            string
		cCharMaxLen, cPrecision, cScale *int
		eTyp                            *string
		eCharMaxLen, ePrecision, eScale *int
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
			&d.eTyp, &d.eCharMaxLen, &d.ePrecision, &d.eScale,
			&isNullable, &d.dflt)
		if err != nil {
			return nil, err
		}
		if isNullable == "YES" {
			d.isNullable = true
		} else {
			d.isNullable = false
		}
		descr = append(descr, &d)
	}

	for _, d := range descr {
		var possibleActions *[]types.DataHandling
		var t string
		switch d.cTyp {
		case "numeric":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
			if d.cPrecision != nil {
				if d.cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
				}
			} else {
				t = "numeric"
			}
		case "character varying":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
		case "character":
			if d.cCharMaxLen != nil {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
			} else {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = "bpchar"
			}
		case "ARRAY":
			switch *d.eTyp {
			case "numeric":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				if d.ePrecision != nil {
					if d.cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)[]", *d.ePrecision, *d.eScale)
					} else {
						t = fmt.Sprintf("numeric(%d)[]", *d.ePrecision)
					}
				} else {
					t = "numeric[]"
				}
			case "character varying":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				if d.eCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)[]", *d.eCharMaxLen)
				} else {
					t = "varchar[]"
				}
			case "character":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				if d.eCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)[]", *d.eCharMaxLen)
				} else {
					t = "character[]"
				}
			default:
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = *d.eTyp + "[]"
			}
		default:
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
			t = d.cTyp
		}
		c := types.Column{
			Name:            d.cName,
			Position:        d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         d.dflt,
			Reference:       nil,
			Semantics:       nil,
			PossibleActions: *possibleActions,
		}
		ti.Columns = append(ti.Columns, c)
	}

	if err = da.resolveRefs(tip, &ti); err != nil {
		return nil, err
	}

	return &ti, nil
}

func (da *Postgres) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {
	rows, err := da.db.Query(`
           SELECT
	       tc.constraint_name, 
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
	   WHERE tc.table_schema = $1 and tc.table_name = $2 and tc.constraint_type = 'FOREIGN KEY'`,
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
