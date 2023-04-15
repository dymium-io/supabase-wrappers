package main

import (
	"database/sql"
	_ "github.com/sijms/go-ora/v2"

	"fmt"
	"net/url"
	"strings"

	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
)

type OracleDB struct {
	db *sql.DB
}

func (da *OracleDB) Ping() error {
	return da.db.Ping()
}

func (da *OracleDB) Close() {
	da.db.Close()
}

func (da *OracleDB) Connect(c *types.ConnectionParams) error {
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
		return err
	}
	if err := db.Ping(); err != nil {
		return err
	}

	da.db = db
	return nil
}

func (da *OracleDB) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {
	rows, err :=
		da.db.Query(`SELECT OWNER, TABLE_NAME
                             FROM ALL_TABLES
                             WHERE TABLESPACE_NAME NOT IN ('SYSTEM', 'SYSAUX', 'UNDOTBS1', 'TEMP')
                             ORDER BY OWNER, TABLE_NAME`)

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

func (da OracleDB) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {

	stmt, err :=
		da.db.Prepare(`SELECT COLUMN_ID,
                                    COLUMN_NAME,
                                    DATA_TYPE,
                                    DATA_LENGTH,
                                    CHAR_LENGTH,
                                    DATA_PRECISION,
                                    DATA_SCALE,
                                    NULLABLE,
                                    DEFAULT_LENGTH,
                                    DATA_DEFAULT
                             FROM ALL_TAB_COLS
                             WHERE OWNER = :1 AND TABLE_NAME = :2
                             ORDER BY COLUMN_ID`)
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
		cDataLen                        int
		pos                             *int
		isNullable                      bool
		cDflt                           *string
		cTyp                            *string
		cCharMaxLen, cPrecision, cScale *int
		dLength                         *int
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable *string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cDataLen, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
			&isNullable, &d.dLength, &d.cDflt)
		if err != nil {
			return nil, err
		}

		if isNullable != nil && *isNullable == "N" {
			d.isNullable = false
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
		if d.cTyp == nil {
			t = "undefined"
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
			semantics = detectors.FindSemantics(d.cName, nil)
		} else {
			switch strings.ToLower(*d.cTyp) {
			case "number":
				if d.cPrecision != nil {
					if d.cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
					} else {
						t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
					}
				} else {
					t = fmt.Sprintf("numeric(%d)", d.cDataLen)
				}
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, nil)
			case "varchar2", "nvarchar2":
				if d.cCharMaxLen != nil && *d.cCharMaxLen > 0 {
					t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
				} else {
					t = "varchar"
				}
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, &(*sample)[k])
			case "char", "nchar":
				if d.cCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
					possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
					semantics = detectors.FindSemantics(d.cName, &(*sample)[k])
				} else {
					t = "bpchar"
					possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
					semantics = detectors.FindSemantics(d.cName, nil)
				}
			case "clob", "nclob", "long":
				t = "text"
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, &(*sample)[k])
			case "blob", "bfile", "long raw":
				t = "bytea"
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, nil)
			case "date":
				t = "date"
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
				semantics = detectors.FindSemantics(d.cName, nil)
			default:
				// ignore other datatypes (e.g. GEOM)
				continue
			}
		}
		if d.pos == nil {
			p := 0
			d.pos = &p
		}
		var dflt *string
		if d.dLength == nil || *d.dLength == 0 || d.cDflt == nil {
			dflt = nil
		} else {
			dd := (*d.cDflt)[:*d.dLength]
			dflt = &dd
		}
		c := types.Column{
			Name:            d.cName,
			Position:        *d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         dflt,
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

func (da *OracleDB) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {
	stmt, err := da.db.Prepare(`
	   SELECT a.CONSTRAINT_NAME, a.COLUMN_NAME,
		  c.R_OWNER AS REF_OWNER, cpk.TABLE_NAME AS REF_TABLE,
                  c.R_CONSTRAINT_NAME
           FROM ALL_TABLES t
	   JOIN ALL_CONS_COLUMNS a ON t.OWNER = a.OWNER
               AND t.TABLE_NAME = a.TABLE_NAME
	   JOIN ALL_CONSTRAINTS c ON a.OWNER = c.OWNER
	       AND a.CONSTRAINT_NAME = c.CONSTRAINT_NAME
	   JOIN ALL_CONSTRAINTS cpk ON c.R_OWNER = cpk.OWNER
	       AND c.R_CONSTRAINT_NAME = cpk.CONSTRAINT_NAME
           WHERE c.OWNER = :1 AND a.TABLE_NAME = :2 AND c.CONSTRAINT_TYPE = 'R'`)
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
			c := &ti.Columns[k]
			if c.Name == columnName {
				c.Reference = &types.Reference{
					Schema: fSchema,
					Table:  fTblName,
					Column: fColumnName,
				}
			}
		}
	}

	return nil
}

func (da *OracleDB) getSample(schema, table string, nColumns int) (*[][]string, error) {

	var percent float32
	{
		r := da.db.QueryRow(fmt.Sprintf(`SELECT COUNT(*) FROM "%s"."%s"`, schema, table))
		var count int
		r.Scan(&count)
		if count == 0 {
			ret := make([][]string, nColumns)
			return &ret, nil
		}
		percent = float32(detect.SampleSize) / float32(count) * 100.0
		if percent >= 100 {
			percent = 99.9999
		}
	}

	rows := make([][]*string, 0, detect.SampleSize)

	sql := fmt.Sprintf(`SELECT * from "%s"."%s" SAMPLE(%f)`, schema, table, percent)
	r, err := da.db.Query(sql)
	if err != nil {
		return nil, err
	}
	defer r.Close()

	i := make([]interface{}, nColumns)
	s := make([]*string, nColumns)
	for k := 0; k != nColumns; k++ {
		i[k] = &s[k]
	}

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
