package main

import (
	"database/sql"
	_ "github.com/go-sql-driver/mysql"

	"fmt"

	"DbAnalyzer/types"
	// "DbAnalyzer/common"
)

func connectMysql(c *types.ConnectionParams) (*sql.DB, error) {
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
	if err := db.Ping(); err != nil {
		return nil, err
	}
	return db, nil
}

func getMysqlInfo(dbName string, db *sql.DB) (*types.DatabaseInfoData, error) {

	rows, err := db.Query(`SELECT table_schema, table_name
                               FROM information_schema.tables
                               ORDER BY table_schema, table_name`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	database := types.DatabaseInfoData{
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


func getMysqlTblInfo(dbName string, tip *types.TableInfoParams, db *sql.DB) (*types.TableInfoData, error) {

	/*
	if c.Rules == nil {
		return struct{}{}, fmt.Errorf("Policy rules are not defined")
	}

	var detectors *common.Detectors
	if detectors,err = common.Compile(*c.Rules); err != nil {
		return struct{}{}, err
	}
	*/

	rows, err := db.Query(`SELECT ordinal_position, column_name,
                                      data_type,
                                      character_maximum_length,
                                      numeric_precision, numeric_scale,
                                      is_nullable, column_default
                               FROM information_schema.columns
                               WHERE table_schema = ? and table_name = ?
                               ORDER BY ordinal_position`, tip.Schema, tip.Table)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	ti := types.TableInfoData{
		DbName:    dbName,
		Schema:    tip.Schema,
		TblName:   tip.Table,
	}

	type data struct {
		cName string
		pos int
		isNullable bool
		dflt *string
		cTyp string
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

	
	for _, d := range descr {
		var t string
		var semantics *string
		var possibleActions *[]types.DataHandling
		switch d.cTyp {
		case "decimal":
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
		case "varchar":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			/*
			if s := common.MatchColumnName(detectors, cName); s != nil {
				semantics = s
			} else {
				// TBD: 
			}
			*/
		case "char":
			if d.cCharMaxLen != nil {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
			} else {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = "bpchar"
			}
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
		default:
			t = d.cTyp
		}

		c := types.Column{
			Name:       d.cName,
			Position:   d.pos,
			Typ:        t,
			IsNullable: d.isNullable,
			Default:    d.dflt,
			Reference:  nil,
			Semantics:  semantics,
			PossibleActions: *possibleActions,
		}
		
		ti.Columns = append(ti.Columns, c)
	}

	if err = resolveMysqlRefs(db, tip, &ti); err != nil {
		return nil, err
	}

	return &ti, nil
}

func resolveMysqlRefs(db *sql.DB, tip *types.TableInfoParams, ti *types.TableInfoData) error {
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
