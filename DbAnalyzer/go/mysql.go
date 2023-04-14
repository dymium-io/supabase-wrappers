package main

import (
	"database/sql"
	_ "github.com/go-sql-driver/mysql"

	"fmt"

	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
)

type MySQL struct {
	db *sql.DB
}

func (da *MySQL) Ping() error {
	return da.db.Ping()
}

func (da *MySQL) Close() {
	da.db.Close()
}

func (da *MySQL) Connect(c *types.ConnectionParams) error {
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
		return err
	}
	if err := db.Ping(); err != nil {
		return err
	}

	da.db = db
	return nil
}

func (da *MySQL) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {

	rows, err := da.db.Query(`SELECT table_schema, table_name
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
			switch schema {
			case "information_schema", "mysql", "sys", "performance_schema":
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

func (da MySQL) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {

	stmt, err :=
		da.db.Prepare(`SELECT ordinal_position, column_name,
                                    data_type,
                                    character_maximum_length,
                                    numeric_precision, numeric_scale,
                                    is_nullable, column_default
                             FROM information_schema.columns
                             WHERE table_schema = ? and table_name = ?
                             ORDER BY ordinal_position`)
	if err != nil {
		return nil, err
	}
	defer stmt.Close()

	rows, err := stmt.Query(tip.Schema, tip.Table)
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
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
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

	detectors, err := detect.Compile(tip.Rules)
	if err != nil {
		return nil, err
	}

	sample, err := da.getSample(tip.Schema, tip.Table, len(descr))
	if err != nil {
		return nil, err
	}

	for k, d := range descr {
		var t string
		var semantics *string
		var possibleActions *[]types.DataHandling
		switch d.cTyp {
		case "decimal":
			if d.cPrecision != nil {
				if d.cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
				}
			} else {
				t = "numeric"
			}
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
			semantics = detectors.FindSemantics(d.cName, nil)
		case "varchar":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
			semantics = detectors.FindSemantics(d.cName, &(*sample)[k])
		case "char":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, &(*sample)[k])
			} else {
				t = "bpchar"
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, nil)
			}
		default:
			t = d.cTyp
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
			semantics = detectors.FindSemantics(d.cName, nil)
		}

		c := types.Column{
			Name:            d.cName,
			Position:        d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         d.dflt,
			Reference:       nil,
			Semantics:       semantics,
			PossibleActions: *possibleActions,
		}

		ti.Columns = append(ti.Columns, c)
	}

	if err = da.resolveRefs(tip, &ti); err != nil {
		return nil, err
	}

	return &ti, nil
}

func (da *MySQL) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {
	stmt, err := da.db.Prepare(`
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
	   WHERE tc.table_schema = ? and tc.table_name = ? and tc.constraint_type = 'FOREIGN KEY'`)
	if err != nil {
		return err
	}
	defer stmt.Close()

	rows, err := stmt.Query(tip.Schema, tip.Table)
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

func (da *MySQL) getSample(schema, table string, nColumns int) (*[][]string, error) {

	rows := make([][]*string, 0, detect.SampleSize)

	i := make([]interface{}, nColumns)
	s := make([]*string, nColumns)
	for k := 0; k != nColumns; k++ {
		i[k] = &s[k]
	}

	sql := fmt.Sprintf("SELECT * from `%s`.`%s` ORDER BY RAND() LIMIT %d", schema, table, detect.SampleSize)
	r, err := da.db.Query(sql)
	if err != nil {
		return nil, err
	}
	defer r.Close()

	for r.Next() {
		if err := r.Scan(i...); err != nil {
			return nil, err
		}
		rows = append(rows, utils.CopyPointers(s))
	}

	scanned := make([][]string, nColumns)
	for k := 0; k != nColumns; k++ {
		scanned[k] = make([]string, 0, len(rows))
		for j := 0; j != len(rows); j++ {
			if rows[j][k] != nil {
				scanned[k] = append(scanned[k], *rows[j][k])
			}
		}
	}

	return &scanned, nil
}
