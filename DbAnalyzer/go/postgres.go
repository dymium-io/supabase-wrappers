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
                            UNION ALL
                            SELECT schemaname AS table_schema, matviewname AS table_name 
                            FROM pg_matviews
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
		da.db.Query(`WITH base AS (
    SELECT
        attr.attnum AS ordinal_position,
        attr.attname AS column_name,
        typ.typtypmod,
        typ.typtype,
        CASE
            WHEN typ.typtype = 'd' THEN typ.typbasetype
            ELSE attr.atttypid
            END AS base_atttypid,
        attr.attnotnull,
        attr.atttypmod,
        pg_get_expr(def.adbin, def.adrelid) AS column_default,
        elemtyp.typtypmod AS elem_typtypmod,
        elemtyp.typtype AS elem_typtype,
        elemtyp.oid AS elem_type_oid,
        cls.relname AS table_name,
        nsp.nspname AS schema_name
    FROM
        pg_attribute attr
            LEFT JOIN pg_attrdef def ON attr.attrelid = def.adrelid AND attr.attnum = def.adnum
            INNER JOIN pg_class cls ON attr.attrelid = cls.oid AND cls.relkind IN ('r', 'm', 'v')
            INNER JOIN pg_type typ ON attr.atttypid = typ.oid
            LEFT JOIN pg_type elemtyp ON elemtyp.oid = typ.typelem
            INNER JOIN pg_namespace nsp ON cls.relnamespace = nsp.oid
    WHERE
            nsp.nspname = $1
      AND cls.relname = $2
      AND attr.attnum > 0
      AND NOT attr.attisdropped
)
SELECT
    ordinal_position,
    column_name,
    pg_catalog.format_type(base_atttypid, NULL) AS data_type,
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = ANY('{char,varchar,text}'::regtype[]) AND atttypmod > 0 THEN atttypmod - 4
        ELSE NULL
        END AS character_maximum_length,
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = ANY('{numeric,decimal}'::regtype[]) AND atttypmod <> -1 THEN (atttypmod - 4) >> 16
        ELSE NULL
        END AS numeric_precision,
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = ANY('{numeric,decimal}'::regtype[]) AND atttypmod <> -1 THEN (atttypmod - 4) & 65535
        ELSE NULL
        END AS numeric_scale,
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = ANY('{timestamp,timestamptz}'::regtype[]) AND atttypmod <> -1 THEN (atttypmod - 4)
        ELSE NULL
        END AS datetime_precision,
    -- Handling for interval data type
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = 'interval'::regtype THEN (atttypmod - 4) & 65535
        ELSE NULL
        END AS interval_precision,
    CASE
        WHEN typtype IN ('b', 'd') AND base_atttypid = 'interval'::regtype THEN
            CASE
                WHEN (atttypmod - 4) >> 16 & 65535 = 0 THEN 'YEAR'
                WHEN (atttypmod - 4) >> 16 & 65535 = 1 THEN 'MONTH'
                WHEN (atttypmod - 4) >> 16 & 65535 = 2 THEN 'DAY'
                WHEN (atttypmod - 4) >> 16 & 65535 = 3 THEN 'HOUR'
                WHEN (atttypmod - 4) >> 16 & 65535 = 4 THEN 'MINUTE'
                WHEN (atttypmod - 4) >> 16 & 65535 = 5 THEN 'SECOND'
                WHEN (atttypmod - 4) >> 16 & 65535 = 6 THEN 'YEAR TO MONTH'
                WHEN (atttypmod - 4) >> 16 & 65535 = 7 THEN 'DAY TO HOUR'
                WHEN (atttypmod - 4) >> 16 & 65535 = 8 THEN 'DAY TO MINUTE'
                WHEN (atttypmod - 4) >> 16 & 65535 = 9 THEN 'DAY TO SECOND'
                WHEN (atttypmod - 4) >> 16 & 65535 = 10 THEN 'HOUR TO MINUTE'
                WHEN (atttypmod - 4) >> 16 & 65535 = 11 THEN 'HOUR TO SECOND'
                WHEN (atttypmod - 4) >> 16 & 65535 = 12 THEN 'MINUTE TO SECOND'
                ELSE NULL
                END
        ELSE NULL
        END AS interval_type,
    pg_catalog.format_type(elem_type_oid, elem_typtypmod) AS elem_data_type,
    CASE
        WHEN elem_typtype IN ('b', 'd') AND elem_type_oid = ANY('{char,varchar,text}'::regtype[]) AND elem_typtypmod > 0 THEN elem_typtypmod - 4
        ELSE NULL
        END AS elem_character_maximum_length,
    COALESCE(
            CASE
                WHEN base_atttypid = ANY('{numeric[],decimal[]}'::regtype[])
                    THEN (atttypmod - 4) >> 16
                END,
            CASE
                WHEN elem_type_oid = ANY('{numeric,decimal}'::regtype[]) AND typtype = 'a'
                    THEN (elem_typtypmod - 4) >> 16
                END
        ) AS elem_numeric_precision,
    COALESCE(
            CASE
                WHEN base_atttypid = ANY('{numeric[],decimal[]}'::regtype[])
                    THEN (atttypmod - 4) & 65535
                END,
            CASE
                WHEN elem_type_oid = ANY('{numeric,decimal}'::regtype[]) AND typtype = 'a'
                    THEN (elem_typtypmod - 4) & 65535
                END
        ) AS elem_numeric_scale,
    CASE
        WHEN elem_typtype IN ('b', 'd') AND elem_type_oid = ANY('{timestamp,timestamptz}'::regtype[]) AND elem_typtypmod <> -1 THEN (elem_typtypmod - 4)
        ELSE NULL
        END AS elem_datetime_precision,
    -- For interval type, precision and type for element are assumed to be the same as the base type. Adjust if needed.
    CASE
        WHEN elem_typtype IN ('b', 'd') AND elem_type_oid = 'interval'::regtype THEN
            CASE
                WHEN (elem_typtypmod - 4) & 65535 <> 0 THEN (elem_typtypmod - 4) & 65535
                ELSE NULL
                END
        ELSE NULL
        END AS elem_interval_precision,
    CASE
        WHEN elem_typtype IN ('b', 'd') AND elem_type_oid = 'interval'::regtype THEN
            CASE
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 0 THEN 'YEAR'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 1 THEN 'MONTH'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 2 THEN 'DAY'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 3 THEN 'HOUR'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 4 THEN 'MINUTE'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 5 THEN 'SECOND'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 6 THEN 'YEAR TO MONTH'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 7 THEN 'DAY TO HOUR'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 8 THEN 'DAY TO MINUTE'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 9 THEN 'DAY TO SECOND'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 10 THEN 'HOUR TO MINUTE'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 11 THEN 'HOUR TO SECOND'
                WHEN (elem_typtypmod - 4) >> 16 & 65535 = 12 THEN 'MINUTE TO SECOND'
                ELSE NULL
                END
        ELSE NULL
        END AS elem_interval_type,
   CASE
        WHEN base_atttypid = ANY('{bit,bit varying}'::regtype[]) THEN
            substring(pg_catalog.format_type(base_atttypid, atttypmod) from '\((\d+)\)')
        ELSE
            NULL
        END AS bit_length,
     CASE
        WHEN attnotnull THEN 'NO'
        ELSE 'YES'
        END AS is_nullable,
    column_default
FROM
    base
ORDER BY
    ordinal_position;
`, tip.Schema, tip.Table)
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
		cBitLength                      *int
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
			&d.cBitLength, &isNullable_, &d.dflt)
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
		if strings.HasSuffix(d.cTyp, "[]") {
			d.cTyp = "ARRAY"
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
			if d.cBitLength != nil && *d.cBitLength > 0 {
				t = fmt.Sprintf("%s(%d)", d.cTyp, *d.cBitLength)
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
		case "timestamp without time zone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("timestamp(%d) without time zone", *d.cdPrecision)
			} else {
				t = "timestamp without time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time without time zone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("time(%d) without time zone", *d.cdPrecision)
			} else {
				t = "time without time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "timestamp with time zone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("timestamp(%d) with time zone", *d.cdPrecision)
			} else {
				t = "timestamp with time zone"
			}
			possibleActions = allowable
			sample[k] = dtk(true)
		case "time with time zone":
			if d.cdPrecision != nil && *d.cdPrecision < 6 {
				t = fmt.Sprintf("time(%d) with time zone", *d.cdPrecision)
			} else {
				t = "time with time zone"
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
			case "bit", "bit varying", "\"bit\"", "\"bit varying\"":
				eTyp := strings.Trim(*d.eTyp, "\"")
				if d.ePrecision != nil && *d.ePrecision > 0 {
					t = fmt.Sprintf("%s(%d)[]", eTyp, *d.ePrecision)
				} else {
					t = eTyp + "[]"
				}
				// Currently we don't support bit(x)[] - need to figure out how to
				// resolve the x in the array
				possibleActions = blocked
				sem = utils.Unsupported
				sample[k] = dtk(false, sem)
			case "numeric":
				possibleActions = allowable
				if d.ePrecision != nil {
					if d.eScale != nil {
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
			case "timestamp without time zone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("timestamp(%d) without time zone[]", *d.edPrecision)
				} else {
					t = "timestamp without time zone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "time without time zone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("time(%d) without time zone[]", *d.edPrecision)
				} else {
					t = "time without time zone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "timestamp with time zone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("timestamp(%d) with time zone[]", *d.edPrecision)
				} else {
					t = "timestamp with time zone[]"
				}
				possibleActions = allowable
				sample[k] = dtk(true)
			case "time with time zone":
				if d.edPrecision != nil && *d.edPrecision < 6 {
					t = fmt.Sprintf("time(%d) with time zone[]", *d.edPrecision)
				} else {
					t = "time with time zone[]"
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
				sem = utils.Unsupported
				sample[k] = dtk(false, sem)
			}
		default:
			possibleActions = blocked
			t = d.cTyp
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
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
