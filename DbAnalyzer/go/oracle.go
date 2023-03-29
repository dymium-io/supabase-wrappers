package main

import (
	"database/sql"
	_ "github.com/sijms/go-ora/v2"

	"fmt"
	"net/url"
	"strings"

	"DbAnalyzer/types"
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
                             ORDER BY c.OWNER, c.TABLE_NAME`)

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

	rows, err :=
		da.db.Query(`SELECT COLUMN_ID,
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
                             WHERE OWNER = $1 and TABLE_NAME = $2
                             ORDER BY COLUMN_ID`, tip.Schema, tip.Table)
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

	for _, d := range descr {
		var t string
		var semantics *string
		var possibleActions *[]types.DataHandling
		if d.cTyp == nil {
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
			t = "undefined"
		} else {
			switch strings.ToLower(*d.cTyp) {
			case "number":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				if d.cPrecision != nil {
					if d.cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
					} else {
						t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
					}
				} else {
					t = fmt.Sprintf("numeric(%d)", d.cDataLen)
				}
			case "varchar2", "nvarchar2":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				if d.cCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
				} else {
					t = "varchar"
				}
			case "char", "nchar":
				if d.cCharMaxLen != nil {
					possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
					t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				} else {
					possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
					t = "bpchar"
				}
			case "clob", "nclob", "long":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate}
				t = "text"
			case "blob", "bfile", "long raw":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = "bytea"
			case "date":
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact}
				t = "date"
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
	rows, err := da.db.Query(`
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
           WHERE c.OWNER = $1 AND a.TABLE_NAME = $2 AND c.CONSTRAINT_TYPE = 'R'`,
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
