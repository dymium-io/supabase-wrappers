package main

import (
	"database/sql"
	"dymium.com/dymium/log"

	_ "github.com/go-sql-driver/mysql"

	"fmt"
	"math/rand"
	"sort"
	"strings"
	"time"

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
	log.Infof("Connect: Address: %s, Port: %d, Database: %s, Tls: %v", c.Address, c.Port, c.Database, c.Tls)
	mysqlconn := fmt.Sprintf("%s:%s@tcp(%s:%d)/?tls=%s",
		c.User, c.Password, c.Address, c.Port,
		func() string {
			if c.Tls {
				// TODO: implement tls.config so we can veryfy the server's certificate
				return "skip-verify"
				//return "true"
			} else {
				return "false"
			}
		}())

	db, err := sql.Open("mysql", mysqlconn)
	if err != nil {
		log.Errorf("Error connecting to %v: %v\n", c, err)
		return err
	}
	if err := db.Ping(); err != nil {
		log.Errorf("Error pinging to %v: %v\n", c, err)
		return err
	}

	da.db = db
	return nil
}

func (da *MySQL) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {
	log.Infof("Get database info: %v\n", dbName)
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
			log.Errorf("Error scanning: %v\n", err)
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
	log.Infof("Getting table info for %s.%s.%s", dbName, tip.Schema, tip.Table)
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
		log.Errorf("Error preparing statement: %v\n", err)
		return nil, err
	}
	defer stmt.Close()

	rows, err := stmt.Query(tip.Schema, tip.Table)
	if err != nil {
		log.Errorf("Error querying: %v\n", err)
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
			log.Errorf("Error scanning: %v\n", err)
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
		log.Errorf("Error compiling rules: %v\n", err)
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
			sample[k] = dtk(false)
		case "enum":
			t = "varchar"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "json":
			t = "json"
			possibleActions = allowable
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
		log.Errorf("Error resolving references: %v\n", err)
		return nil, err
	}

	if err = da.getSample(tip.Schema, tip.Table, sample); err != nil {
		log.Errorf("Error getting sample: %v\n", err)
		return nil, err
	}

	if err = detectors.FindSemantics(sample); err != nil {
		log.Errorf("Error finding semantics: %v\n", err)
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
	log.Infof("Resolving references for %s.%s.%s", ti.DbName, ti.Schema, ti.TblName)
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
		log.Errorf("Error preparing statement: %v\n", err)
		return err
	}
	defer stmt.Close()

	rows, err := stmt.Query(tip.Schema, tip.Table)
	if err != nil {
		log.Errorf("Error querying: %v\n", err)
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
	log.Infof("Getting sample for %s.%s", schema, table)
	nColumns := len(sample)

	var count int
	{
		r := da.db.QueryRow(fmt.Sprintf("SELECT COUNT(*) FROM `%s`.`%s`",
			schema, table))
		r.Scan(&count)
		if count <= 0 {
			for _, s := range sample {
				s.Data = make([]*string, 0)
			}
			return nil
		}
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
			colNames.WriteString("`" + sample[k].Name + "`")
		}
	}

	sql := fmt.Sprintf("SELECT %s FROM `%s`.`%s` LIMIT %d",
		colNames.String(), schema, table, detect.LimitSize)
	r, err := da.db.Query(sql)
	if err != nil {
		log.Errorf("Error querying: %v\n", err)
		return err
	}
	defer r.Close()

	selected := generateRandomNumbers(utils.Min(detect.LimitSize, count),
		detect.SampleSize)
	iter, ks := 0, 0

	for r.Next() {
		if ks == len(selected) {
			break
		}

		if iter != selected[ks] {
			iter += 1
			continue
		}

		// case of: iter == selected[ks]
		iter += 1
		ks += 1

		if err := r.Scan(i...); err != nil {
			log.Errorf("Error scanning: %v\n", err)
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

func generateRandomNumbers(N, n int) []int {
	numbers := make([]int, N)
	for i := 0; i < N; i++ {
		numbers[i] = i
	}

	if N <= n {
		return numbers
	}

	// Shuffle the numbers
	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})

	// Select the first 32 numbers
	selectedNumbers := numbers[:n]

	// Sort the selected numbers
	sort.Ints(selectedNumbers)

	return selectedNumbers
}
