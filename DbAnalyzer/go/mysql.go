package main

import (
	"database/sql"

	_ "github.com/go-sql-driver/mysql"

	"fmt"
	"strings"

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
                                    datetime_precision,
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
		cName                                       string
		pos                                         int
		isNullable                                  bool
		dflt                                        *string
		cTyp                                        string
		cCharMaxLen, cPrecision, cScale, dPrecision *int
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable_ string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
			&d.dPrecision,
			&isNullable_, &d.dflt)
		if err != nil {
			return nil, err
		}
		if isNullable_ == "YES" {
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

	sample := make([]detect.Sample, len(descr))

	obfuscatable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
	allowable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
	blocked := &[]types.DataHandling{types.DH_Block}

	for k, d := range descr {
		var t string
		var sem *string
		var possibleActions *[]types.DataHandling
		dtk := func(isSamplable bool) detect.Sample {
			return detect.Sample{
				IsSamplable: isSamplable,
				IsNullable:  d.isNullable,
				Name:        d.cName,
			}
		}
		switch d.cTyp {
		case "smallint", "int", "bigint":
			t = d.cTyp
			possibleActions = allowable
			sample[k] = dtk(true)
		case "tinyint", "tinyint unsigned":
			t = "smallint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "smallint unsigned", "mediumint", "mediumint unsigned":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "int unsigned":
			t = "bigint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bigint unsigned":
			t = "numeric(20)"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "decimal", "numeric":
			if d.cPrecision != nil {
				if d.cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
				}
			} else {
				t = "numeric"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "date":
			t = "date"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "datetime":
			if d.dPrecision != nil {
				t = fmt.Sprintf("timestamp (%d) without time zone", *d.dPrecision)
			} else {
				t = "timestamp without time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time":
			if d.dPrecision != nil {
				t = fmt.Sprintf("time (%d) with time zone", *d.dPrecision)
			} else {
				t = "time with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "timestamp":
			if d.dPrecision != nil {
				t = fmt.Sprintf("time (%d) with time zone", *d.dPrecision)
			} else {
				t = "time with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "year":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "varchar", "long", "long varchar":
			if d.cCharMaxLen != nil && *d.cCharMaxLen <= 10485760 {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "text", "mediumtext", "longtext", "tinytext":
			if d.cCharMaxLen != nil && *d.cCharMaxLen <= 10485760 {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "char":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				possibleActions = obfuscatable
			} else {
				t = "bpchar"
				possibleActions = allowable
			}
			sample[k] = dtk(true)
		case "binary", "varbinary", "blob", "tinyblob", "mediumblob", "longblob":
			t = "bytea"
			possibleActions = allowable
			// t = d.cTyp
			// possibleActions = blocked
			// sem = utils.Unsupported
			sample[k] = dtk(false)
		case "enum":
			t = "varchar"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "json":
			t = "json"
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		default:
			switch {
			case strings.HasPrefix(d.cTyp, "real"):
				t = "real"
				possibleActions = allowable
				sample[k] = dtk(true)
			case strings.HasPrefix(d.cTyp, "float") || strings.HasPrefix(d.cTyp, "double"):
				if d.cPrecision != nil && *d.cPrecision <= 24 {
					t = "real"
				} else {
					t = "double precision"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			default:
				t = d.cTyp
				possibleActions = blocked
				sem = utils.Unsupported
				sample[k] = dtk(false)
			}
		}

		c := types.Column{
			Name:            d.cName,
			Position:        d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         d.dflt,
			Reference:       nil,
			Semantics:       sem,
			PossibleActions: *possibleActions,
		}

		ti.Columns = append(ti.Columns, c)
	}

	if err = da.resolveRefs(tip, &ti); err != nil {
		return nil, err
	}

	if err = da.getSample(tip.Schema, tip.Table, sample); err != nil {
		return nil, err
	}

	if err = detectors.FindSemantics(sample); err != nil {
		return nil, err
	}

	for k := range ti.Columns {
		if sample[k].Semantics != nil {
			ti.Columns[k].Semantics = sample[k].Semantics
		}
	}

	return &ti, nil
}

func (da *MySQL) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {
	stmt, err := da.db.Prepare(`
           SELECT
	       tc.constraint_name, 
	       kcu.column_name, 
	       kcu.referenced_table_schema AS foreign_table_schema,
	       kcu.referenced_table_name AS foreign_table_name,
	       kcu.referenced_column_name AS foreign_column_name
	   FROM 
	       information_schema.table_constraints AS tc 
	       JOIN information_schema.key_column_usage AS kcu
		 ON tc.constraint_name = kcu.constraint_name
		 AND tc.table_schema = kcu.table_schema
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

func (da *MySQL) getSample(schema, table string, sample []detect.Sample) error {

	nColumns := len(sample)

	for _, s := range sample {
		if s.IsSamplable {
			s.Data = make([]*string, 0, detect.SampleSize)
		} else {
			s.Data = make([]*string, 0)
		}
	}

	i := make([]interface{}, 0, nColumns)
	s := make([]string, nColumns)
	p := make([]*string, nColumns)
	var colNames strings.Builder
	start := true
	for k := 0; k != nColumns; k++ {
		if sample[k].IsSamplable {
			if sample[k].IsNullable {
				i = append(i, &p[k])
			} else {
				i = append(i, &s[k])
			}
			if start {
				start = false
			} else {
				colNames.WriteString(", ")
			}
			colNames.WriteString("`" + sample[k].Name + "`")
		}
	}

	sql := fmt.Sprintf("SELECT %s from `%s`.`%s` ORDER BY RAND() LIMIT %d",
		colNames.String(), schema, table, detect.SampleSize)
	r, err := da.db.Query(sql)
	if err != nil {
		return err
	}
	defer r.Close()

	for r.Next() {
		if err := r.Scan(i...); err != nil {
			return err
		}

		for k := range sample {
			if sample[k].IsSamplable {
				if sample[k].IsNullable {
					if p[k] == nil {
						sample[k].Data = append(sample[k].Data, nil)
					} else {
						v := (*p[k])[:]
						sample[k].Data = append(sample[k].Data, &v)
					}
				} else {
					v := s[k][:]
					sample[k].Data = append(sample[k].Data, &v)
				}
			}
		}
	}

	return nil
}
