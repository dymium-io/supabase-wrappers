package main

import (
	"database/sql"
	"github.com/apex/log"
	_ "github.com/sijms/go-ora/v2"
	go_ora "github.com/sijms/go-ora/v2"

	"fmt"
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
	/*
		query := url.Values{}
		if c.Tls {
			query.Add("ssl", "true")
			query.Add("ssl verify", "false")
		} else {
			query.Add("ssl", "false")
		}
		u := &url.URL{
			Scheme:   "oracle",
			User:     url.UserPassword(c.User, c.Password),
			Host:     fmt.Sprintf("%s:%d", c.Address, c.Port),
			Path:     strings.ToUpper(c.Database),
			RawQuery: query.Encode(),
		}
		oracleconn := u.String()
	*/

	//port := 2484
	urlOptions := map[string]string{}
	if c.Tls {
		urlOptions["ssl"] = "true"
		urlOptions["SSL VERIFY"] = "false"
		//"wallet": "path to folder that contains oracle wallet",
	} else {
		urlOptions["ssl"] = "false"
	}

	oracleconn := go_ora.BuildUrl(c.Address, c.Port, c.Database, c.User, c.Password, urlOptions)

	db, err := sql.Open("oracle", oracleconn)
	if err != nil {
		log.Errorf("Error connecting to Oracle: %s", err.Error())
		return err
	}
	if err := db.Ping(); err != nil {
		log.Errorf("Error pinging Oracle: %s", err.Error())
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
                            UNION ALL
							SELECT
    							OWNER,
    							VIEW_NAME AS TABLE_NAME
							FROM
    							ALL_VIEWS
							WHERE
        						OWNER NOT IN (
			                    	'SYS', 'SYSTEM', 'CTXSYS', 'DBSNMP', 'DIP', 'AUDSYS',
           			           		'EXFSYS', 'MDSYS', 'MGMT_VIEW', 'OLAPSYS', 'DVSYS',
                      				'OWBSYS', 'ORDPLUGINS', 'ORDSYS', 'OUTLN', 'ORDDATA',
                      				'SI_INFORMTN_SCHEMA', 'SYSMAN', 'TSMSYS', 'LBACSYS',
                      				'WK_TEST', 'WKSYS', 'WMSYS', 'XDB', 'APEX_PUBLIC_USER',
                      				'GSMADMIN_INTERNAL', 'GSMCATUSER', 'GSMUSER', 'SPATIAL_CSW_ADMIN_USR'
        						)
  								AND
    								VIEW_NAME NOT LIKE 'BIN$%'
  								AND
        							VIEW_NAME NOT LIKE 'AQ$%'
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
		da.db.Prepare(`SELECT 
    							COLUMN_ID,
    							COLUMN_NAME,
    							DATA_TYPE,
    							DECODE(DATA_TYPE, 'VARCHAR2', CHAR_LENGTH, 'CHAR', CHAR_LENGTH, DATA_LENGTH) AS DATA_LENGTH, -- Use CHAR_LENGTH for VARCHAR2 and CHAR types
    							CHAR_LENGTH,
    							DATA_PRECISION,
    							DATA_SCALE,
    							NULLABLE,
								DEFAULT_LENGTH,
    							DATA_DEFAULT
							FROM 
    							ALL_TAB_COLUMNS
							WHERE 
    							OWNER = :1 AND TABLE_NAME = :2
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
		var isNullable_ *string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cDataLen, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
			&isNullable_, &d.dLength, &d.cDflt)
		if err != nil {
			return nil, err
		}

		if isNullable_ != nil && *isNullable_ == "N" {
			d.isNullable = false
		} else {
			d.isNullable = true
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
		var possibleActions *[]types.DataHandling
		var sem *string
		dtk := func(isSamplable bool, sem ...*string) detect.Sample {
			var s *string
			if len(sem) == 1 {
				s = sem[0]
			}
			return detect.Sample{
				IsSamplable: isSamplable,
				IsNullable:  d.isNullable,
				Name:        d.cName,
				Semantics:   s,
			}
		}
		if d.cTyp == nil {
			t = "undefined"
			sem = utils.Unsupported
			possibleActions = blocked
			sample[k] = dtk(false)
		} else {
			cTyp := strings.ToLower(*d.cTyp)
			switch cTyp {
			case "number":
				switch {
				case d.cPrecision == nil || *d.cPrecision == 0:
					t = "numeric"
				case d.cScale == nil || *d.cScale == 0:
					switch {
					case *d.cPrecision < 5:
						t = "smallint"
					case *d.cPrecision < 10:
						t = "integer"
					case *d.cPrecision < 19:
						t = "bigint"
					default:
						t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
					}
				default:
					if *d.cPrecision < *d.cScale {
						t = fmt.Sprintf("numeric(%d, %d)", *d.cScale, *d.cScale)
					} else {
						t = fmt.Sprintf("numeric(%d, %d)", *d.cPrecision, *d.cScale)
					}
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "float":
				if d.cPrecision != nil && *d.cPrecision < 54 {
					t = fmt.Sprintf("float(%d)", *d.cPrecision)
				} else {
					t = "numeric"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "binary_float":
				t = "real"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "binary_double":
				t = "double precision"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "varchar", "varchar2", "nvarchar2":
				if d.cCharMaxLen != nil && *d.cCharMaxLen > 0 {
					t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
				} else {
					t = "varchar"
				}
				possibleActions = obfuscatable
				sample[k] = dtk(true)
			case "char", "nchar":
				if d.cCharMaxLen != nil {
					t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
					possibleActions = obfuscatable
				} else {
					t = "bpchar"
					possibleActions = allowable
				}
				sample[k] = dtk(true)
			case "clob", "nclob", "long":
				t = "text"
				possibleActions = obfuscatable
				sample[k] = dtk(true)
			case "blob", "bfile", "long raw":
				t = "bytea"
				possibleActions = allowable
				sample[k] = dtk(false)
			case "date":
				t = "timestamp(0) without time zone"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "xmltype":
				t = "xml"
				possibleActions = allowable
				sample[k] = dtk(false) // GoLang driver does not support XMLTYPE
			default:
				switch {
				case strings.HasPrefix(cTyp, "timestamp"):
					switch {
					case utils.Timestamp_r.MatchString(cTyp):
						if d.cScale != nil {
							if *d.cScale > 6 {
								t = "timestamp(6) without time zone"
							} else {
								t = fmt.Sprintf("timestamp(%d) without time zone", *d.cScale)
							}
						} else {
							t = "timestamp without time zone"
						}
						possibleActions = allowable
						sample[k] = dtk(true)
					case utils.Timestamp_with_zone_r.MatchString(cTyp):
						if d.cScale != nil {
							if *d.cScale > 6 {
								t = "timestamp(6) with time zone"
							} else {
								t = fmt.Sprintf("timestamp(%d) with time zone", *d.cScale)
							}
						} else {
							t = "timestamp with time zone"
						}
						possibleActions = allowable
						sample[k] = dtk(true)
					default:
						t = *d.cTyp
						sem = utils.Unsupported
						possibleActions = blocked
						sample[k] = dtk(false, sem)
					}
				case strings.HasPrefix(cTyp, "interval"):
					switch {
					case utils.Interval_year_r.MatchString(cTyp):
						t = "interval year to month"
						possibleActions = allowable
						sample[k] = dtk(true)
					case utils.Interval_day_r.MatchString(cTyp):
						if d.cScale != nil {
							t = fmt.Sprintf("interval day to second (%d)", *d.cScale)
						} else {
							t = "interval day to second"
						}
						possibleActions = allowable
						sample[k] = dtk(true)
					default:
						t = *d.cTyp
						sem = utils.Unsupported
						possibleActions = blocked
						sample[k] = dtk(false, sem)
					}
				case strings.HasPrefix(cTyp, "raw"):
					t = "bytea"
					possibleActions = allowable
					sample[k] = dtk(false)
				default:
					t = *d.cTyp
					sem = utils.Unsupported
					possibleActions = blocked
					sample[k] = dtk(false, sem)
				}
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

func (da *OracleDB) getSample(schema, table string, sample []detect.Sample) error {

	nColumns := len(sample)

	r := da.db.QueryRow(fmt.Sprintf(`SELECT COUNT(*) FROM "%s"."%s"`, schema, table))
	var count int
	r.Scan(&count)
	if count <= 0 {
		for _, s := range sample {
			s.Data = make([]*string, 0)
		}
		return nil
	}

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
			colNames.WriteString(`"` + sample[k].Name + `"`)
		}
	}

	sql := fmt.Sprintf(`SELECT * FROM (SELECT %s from "%s"."%s" WHERE ROWNUM <= %d) WHERE ROWNUM < %d ORDER BY DBMS_RANDOM.VALUE`,
		colNames.String(), schema, table, detect.LimitSize, detect.SampleSize)
	rows, err := da.db.Query(sql)
	if err != nil {
		return err
	}
	defer rows.Close()

	for rows.Next() {
		if err := rows.Scan(i...); err != nil {
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
