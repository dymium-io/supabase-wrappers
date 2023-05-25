package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"strings"

	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
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
                                    c.datetime_precision,
                                    c.interval_precision, c.interval_type,
                                    e.data_type,
                                    e.character_maximum_length,
                                    e.numeric_precision, e.numeric_scale,
                                    e.datetime_precision,
                                    e.interval_precision, e.interval_type,
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
		cdPrecision, ciPrecision        *int
		cIntervalType                   *string
		eTyp                            *string
		eCharMaxLen, ePrecision, eScale *int
		edPrecision, eiPrecision        *int
		eIntervalType                   *string
	}

	descr := []*data{}

	for rows.Next() {
		var d data
		var isNullable_ string
		err = rows.Scan(&d.pos, &d.cName,
			&d.cTyp, &d.cCharMaxLen, &d.cPrecision, &d.cScale,
			&d.cdPrecision, &d.ciPrecision, &d.cIntervalType,
			&d.eTyp, &d.eCharMaxLen, &d.ePrecision, &d.eScale,
			&d.edPrecision, &d.eiPrecision, &d.eIntervalType,
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
		var possibleActions *[]types.DataHandling
		var t string
		var sem *string
		dtk := func(isSamplable bool) detect.Sample {
			return detect.Sample{
				IsSamplable: isSamplable,
				IsNullable:  d.isNullable,
				Name:        d.cName,
			}
		}
		switch d.cTyp {
		case "boolean",
			"smallint", "integer", "bigint", "int", "int2", "int4", "int8",
			"real", "double precision", "float4", "float8", "money",
			"json", "jsonb", "xml",
			"inet", "cidr", "macaddr", "macaddr8",
			"date",
			"uuid",
			"point", "line", "lseg", "box", "path", "polygon", "circle":
			t = d.cTyp
			possibleActions = allowable
			sample[k] = dtk(true)
		case "smallserial":
			t = "smallint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "serial":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bigserial":
			t = "bigint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bit", "bit varying":
			if d.cPrecision != nil && *d.cPrecision > 0 {
				t = fmt.Sprintf("%s(%d)", d.cTyp, *d.cPrecision)
			} else {
				t = d.cTyp
			}
			possibleActions = allowable
			sample[k] = dtk(false)
		case "numeric":
			possibleActions = allowable
			if d.cPrecision != nil && *d.cPrecision > 0 {
				if d.cScale != nil && *d.cScale >= 0 {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
				}
			} else {
				t = "numeric"
			}
			sample[k] = dtk(true)
		case "decimal":
			possibleActions = allowable
			if d.cPrecision != nil && *d.cPrecision > 0 {
				if d.cScale != nil && *d.cScale >= 0 {
					t = fmt.Sprintf("decimal(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("decimal(%d)", *d.cPrecision)
				}
			} else {
				t = "decimal"
			}
			sample[k] = dtk(true)
		case "float":
			possibleActions = allowable
			if d.cPrecision != nil && *d.cPrecision > 0 {
				t = fmt.Sprintf("float(%d)", *d.cPrecision)
			} else {
				t = "double precision"
			}
			sample[k] = dtk(true)
		case "character varying":
			possibleActions = obfuscatable
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			sample[k] = dtk(true)
		case "text":
			possibleActions = obfuscatable
			t = "text"
			sample[k] = dtk(true)
		case "character":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				if *d.cCharMaxLen > 1 {
					possibleActions = obfuscatable
				} else {
					possibleActions = allowable
				}
			} else {
				possibleActions = obfuscatable
				t = "bpchar"
			}
			sample[k] = dtk(true)
		case "bpchar":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				if *d.cCharMaxLen > 1 {
					possibleActions = obfuscatable
				} else {
					possibleActions = allowable
				}
			} else {
				possibleActions = obfuscatable
				t = "bpchar"
			}
			sample[k] = dtk(true)
		case "timestamp without timezone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("timestamp(%d) without timezone", *d.cdPrecision)
			} else {
				t = "timestamp without timezone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time without timezone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("time(%d) without timezone", *d.cdPrecision)
			} else {
				t = "time without timezone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "timestamp with timezone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("timestamp(%d) with timezone", *d.cdPrecision)
			} else {
				t = "timestamp with timezone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time with timezone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("time(%d) with timezone", *d.cdPrecision)
			} else {
				t = "time with timezone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "interval":
			if d.cIntervalType != nil {
				t = fmt.Sprintf("interval %s", *d.cIntervalType)
			} else {
				t = "interval"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bytea":
			t = d.cTyp
			possibleActions = allowable
			sample[k] = dtk(false)
		case "ARRAY":
			switch *d.eTyp {
			case "boolean",
				"smallint", "integer", "bigint", "int", "int2", "int4", "int8",
				"real", "double precision", "float4", "float8", "money",
				"json", "jsonb", "xml",
				"inet", "cidr", "macaddr", "macaddr8",
				"date",
				"uuid",
				"point", "line", "lseg", "box", "path", "polygon", "circle":
				t = *d.eTyp + "[]"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "bit", "bit varying":
				if d.ePrecision != nil && *d.ePrecision > 0 {
					t = fmt.Sprintf("%s(%d)[]", *d.eTyp, *d.ePrecision)
				} else {
					t = *d.eTyp + "[]"
				}
				possibleActions = allowable
				sample[k] = dtk(false)
			case "numeric":
				possibleActions = allowable
				if d.ePrecision != nil {
					if d.cScale != nil {
						t = fmt.Sprintf("numeric(%d,%d)[]", *d.ePrecision, *d.eScale)
					} else {
						t = fmt.Sprintf("numeric(%d)[]", *d.ePrecision)
					}
				} else {
					t = "numeric[]"
				}
				sample[k] = dtk(true)
			case "decimal":
				if d.ePrecision != nil && *d.ePrecision > 0 {
					if d.eScale != nil && *d.eScale >= 0 {
						t = fmt.Sprintf("decimal(%d,%d)[]", *d.ePrecision, *d.eScale)
					} else {
						t = fmt.Sprintf("decimal(%d)[]", *d.ePrecision)
					}
				} else {
					t = "decimal[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "float":
				possibleActions = allowable
				if d.ePrecision != nil && *d.ePrecision > 0 {
					t = fmt.Sprintf("float(%d)[]", *d.ePrecision)
				} else {
					t = "double precision[]"
				}
				sample[k] = dtk(true)
			case "character varying":
				if d.eCharMaxLen != nil {
					t = fmt.Sprintf("varchar(%d)[]", *d.eCharMaxLen)
				} else {
					t = "varchar[]"
				}
				possibleActions = obfuscatable
				sample[k] = dtk(true)
			case "character":
				if d.eCharMaxLen != nil {
					possibleActions = obfuscatable
					t = fmt.Sprintf("character(%d)[]", *d.eCharMaxLen)
				} else {
					possibleActions = obfuscatable
					t = "bpchar[]"
				}
				sample[k] = dtk(true)
			case "text":
				possibleActions = obfuscatable
				t = "text[]"
				sample[k] = dtk(true)
			case "timestamp without timezone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("timestamp(%d) without timezone[]", *d.edPrecision)
				} else {
					t = "timestamp without timezone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "time without timezone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("time(%d) without timezone[]", *d.edPrecision)
				} else {
					t = "time without timezone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "timestamp with timezone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("timestamp(%d) with timezone[]", *d.edPrecision)
				} else {
					t = "timestamp with timezone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "time with timezone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("time(%d) with timezone[]", *d.edPrecision)
				} else {
					t = "time with timezone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "interval":
				if d.eIntervalType != nil {
					t = fmt.Sprintf("interval %s[]", *d.eIntervalType)
				} else {
					t = "interval[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "bytea":
				t = "bytea[]"
				possibleActions = allowable
				sample[k] = dtk(false)
			default:
				possibleActions = blocked
				t = *d.eTyp + "[]"
				sample[k] = dtk(false)
				sem = utils.Unsupported
			}
		default:
			possibleActions = blocked
			t = d.cTyp
			sample[k] = dtk(false)
			sem = utils.Unsupported
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

func (da *Postgres) getSample(schema, table string, sample []detect.Sample) error {

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
			colNames.WriteString(`"` + sample[k].Name + `"`)
		}
	}

	sql := fmt.Sprintf(`SELECT %s from "%s"."%s" ORDER BY RANDOM() LIMIT %d`,
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
