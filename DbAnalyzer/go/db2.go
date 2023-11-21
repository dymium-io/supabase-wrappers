package main

import (
	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
	"dymium.com/dymium/log"
	"fmt"
	//_ "github.com/alexbrainman/odbc"
	_ "github.com/ibmdb/go_ibm_db"
	"strings"

	"github.com/jmoiron/sqlx"
)

type DB2 struct {
	db *sqlx.DB
}

func (da *DB2) Ping() error {
	return da.db.Ping()
}

func (da *DB2) Close() {
	da.db.Close()
}

func (da *DB2) Connect(c *types.ConnectionParams) error {
	db2conn := fmt.Sprintf("HOSTNAME=%s;PORT=%d;DATABASE=%s;UID=%s;PWD=%s;PROTOCOL=TCPIP;Driver=/var/lib/postgresql/sqllib/lib64/libdb2o.so",
		c.Address, c.Port, c.Database, c.User, c.Password)

	if c.Tls {
		//TODO: DB2 SSL-based auth, we might have to use a connection string with additional parameters: SSLClientKeystoredb and SSLClientKeystash
		db2conn = fmt.Sprintf("%s;Security=SSL", db2conn)
	}
	db, err := sqlx.Open("go_ibm_db", db2conn)
	if err != nil {
		log.Errorf("cannot connect to go_ibm_db driver, error: [%+v]", err)
		return err
	}
	if err := db.Ping(); err != nil {
		log.Errorf("cannot ping go_ibm_db driver, error: [%+v]", err)
		return err
	}

	da.db = db
	return nil
}

func (da *DB2) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {

	rows, err :=
		da.db.Query(`select tabschema, tabname, owner, ownertype 
							from syscat.tables
                             ORDER BY tabschema, tabname`)
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
		var schema, tblName, owner, ownertype string
		err = rows.Scan(&schema, &tblName, &owner, &ownertype)
		if err != nil {
			return nil, err
		}
		if curSchema == -1 || schema != database.Schemas[curSchema].Name {
			switch ownertype {
			case "s", "S":
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

func (da DB2) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {
	// TODO: add support for DEFAULT values.
	// We need this to support write ops. For now, we'll just return NULLs for all columns.
	// Current implementation returns "unsupported column type -99". DEFAULT type us CLOB.
	rows, err :=
		da.db.Queryx(`select 
       							c.colno,
       							c.colname,
       							c.typename,
       							c.length,
       							c.scale,     
       							c.nulls
							from syscat.columns c
							where tabschema = ? and tabname = ?
							order by colno`, tip.Schema, tip.Table)
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
		cName           string
		pos             int
		isNullable      bool
		dflt            *string
		cTyp            string
		cLength, cScale *int
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable_ string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cLength,
			&d.cScale,
			&isNullable_)
		if err != nil {
			return nil, err
		}

		if isNullable_ == "Y" {
			d.isNullable = true
		} else {
			d.isNullable = false
		}
		descr = append(descr, &d)
		log.Infof("%+v\n", d)
		//fmt.Printf("%+v\n", d)
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
		var possibleActions *[]types.DataHandling
		var t string
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
		switch strings.ToLower(d.cTyp) {
		case "smallint":
			t = "smallint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "integer":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bigint":
			t = "bigint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "decimal", "numeric", "decfloat":
			if d.cLength != nil {
				if d.cScale != nil {
					t = fmt.Sprintf("decimal(%d,%d)", *d.cLength, *d.cScale)
				} else {
					t = fmt.Sprintf("decimal(%d)", *d.cLength)
				}
			} else {
				t = "numeric"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "real", "float":
			t = "real"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "double":
			t = "double precision"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "date":
			t = "date"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time":
			switch {
			case d.cScale == nil:
				t = "time"
			case *d.cScale == 0:
				t = "time"
			case *d.cScale > 6:
				t = "time (6) without time zone"
			default:
				t = fmt.Sprintf("time (%d) without time zone", *d.cScale)
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "timestamp":
			if d.cScale != nil {
				t = fmt.Sprintf("timestamp (%d) with time zone", *d.cScale)
			} else {
				t = "time with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "varchar", "vargraphic":
			if d.cLength != nil && *d.cLength > 0 {
				t = fmt.Sprintf("varchar(%d)", *d.cLength)
			} else {
				t = "text"
			}
			possibleActions = obfuscatable
			sample[k] = dtk(true)
		case "char", "character", "graphic", "boolean":
			if d.cLength != nil && *d.cLength > 0 {
				possibleActions = obfuscatable
				t = fmt.Sprintf("character(%d)", *d.cLength)
			} else {
				possibleActions = allowable
				t = "bpchar"
			}
			sample[k] = dtk(true)
		case "text":
			possibleActions = obfuscatable
			t = "text"
			sample[k] = dtk(true)
		case "clob":
			t = "text"
			possibleActions = obfuscatable
			sample[k] = dtk(false)
		case "dbclob":
			t = "text"
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		case "binary", "varbinary", "blob":
			possibleActions = allowable
			t = "bytea"
			sample[k] = dtk(false)
		case "xml":
			t = "xml"
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		default:
			switch {
			case strings.HasPrefix(d.cTyp, "real"):
				t = "real"
				possibleActions = allowable
				sample[k] = dtk(true)
			case strings.HasPrefix(d.cTyp, "float"):
				if d.cLength != nil {
					t = fmt.Sprintf("float(%d)", *d.cLength)
				} else {
					t = "real"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			default:
				t = d.cTyp
				possibleActions = blocked
				sem = utils.Unsupported
				sample[k] = dtk(false, sem)
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

func (da *DB2) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {
	log.Infof("Resolving references for table %s.%s", tip.Schema, tip.Table)
	rows, err := da.db.Query(`SELECT
                    rfc.CONSTNAME as constraintName,
                    kcu.COLNAME as columnName,
                    kcu1.TABSCHEMA as fSchema,
                    kcu1.TABNAME as fTblName,
                    kcu1.COLNAME as fColumnName
                FROM SYSCAT.REFERENCES rfc
                    INNER JOIN SYSCAT.KEYCOLUSE kcu ON kcu.CONSTNAME = rfc.CONSTNAME
                    INNER JOIN SYSCAT.KEYCOLUSE kcu1 ON kcu1.CONSTNAME = rfc.REFKEYNAME
                WHERE rfc.TABNAME = ? AND rfc.TABSCHEMA = ?`, tip.Schema, tip.Table)
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

func (da *DB2) getSample(schema, table string, sample []detect.Sample) error {

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
			//colNames.WriteString("[" + sample[k].Name + "]")
			colNames.WriteString("\"" + sample[k].Name + "\"")
		}
	}

	//SELECT %s from "%s"."%s" tablesample bernoulli (%d)
	sql := fmt.Sprintf(`SELECT * FROM (SELECT %s FROM "%s"."%s" FETCH FIRST %d ROWS ONLY) ORDER BY RAND() FETCH FIRST %d ROWS ONLY`,
		colNames.String(), schema, table, detect.LimitSize, detect.SampleSize)
	log.Infof("sql: %s", sql)
	r, err := da.db.Query(sql)
	if err != nil {
		log.Errorf("error: %v", err)
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
