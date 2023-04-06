package main

import (
	"database/sql"
	//_ "github.com/thda/tds"
	"fmt"
	_ "github.com/denisenkom/go-mssqldb"
	"net/url"

	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
)

type SqlServer struct {
	db *sql.DB
}

func (da *SqlServer) Ping() error {
	return da.db.Ping()
}

func (da *SqlServer) Close() {
	da.db.Close()
}

func (da *SqlServer) Connect(c *types.ConnectionParams) error {
	query := url.Values{}
	query.Add("database", c.Database)
	if c.Tls {
		query.Add("encrypt", "true")
	} else {
		query.Add("encrypt", "disable")
	}
	u := &url.URL{
		Scheme: "sqlserver",
		User:   url.UserPassword(c.User, c.Password),
		Host:   fmt.Sprintf("%s:%d", c.Address, c.Port),
		// Path:  instance, // if connecting to an instance instead of a port
		RawQuery: query.Encode(),
	}
	tdsconn := u.String()

	fmt.Println("tdsconn:", tdsconn)
	db, err := sql.Open("sqlserver", tdsconn)
	if err != nil {
		return err
	}
	if err := db.Ping(); err != nil {
		return err
	}

	da.db = db
	return nil
}

func (da *SqlServer) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {

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
			switch schema {
			case "information_schema", "sys", "performance_schema":
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
						IsSystem: isSystem || da.isSysTable(tblName),
					},
				},
			})
			curSchema += 1
		} else {
			database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
				types.Table{
					Name:     tblName,
					IsSystem: isSystem || da.isSysTable(tblName),
				})
		}
	}

	return &database, nil
}

func (da SqlServer) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {

	rows, err :=
		da.db.Query(`SELECT ordinal_position, column_name,
                                    data_type,
                                    character_maximum_length,
                                    numeric_precision, numeric_scale,
                                    is_nullable, column_default
                             FROM information_schema.columns
                             WHERE table_schema = @p1 and table_name = @p2
                             ORDER BY ordinal_position`, tip.Schema, tip.Table)
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
		var possibleActions *[]types.DataHandling
		var t string
		var semantics *string
		switch d.cTyp {
		case "decimal":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
			if d.cPrecision != nil {
				if d.cScale != nil {
					t = fmt.Sprintf("numeric(%d,%d)", *d.cPrecision, *d.cScale)
				} else {
					t = fmt.Sprintf("numeric(%d)", *d.cPrecision)
				}
			} else {
				t = "numeric"
			}
		case "varchar", "nvarchar":
			if d.cCharMaxLen != nil {
				t = fmt.Sprintf("varchar(%d)", *d.cCharMaxLen)
			} else {
				t = "varchar"
			}
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
			semantics = detectors.FindSemantics(d.cName, (*sample)[k])
		case "char","nchar":
			if d.cCharMaxLen != nil {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
				t = fmt.Sprintf("character(%d)", *d.cCharMaxLen)
				semantics = detectors.FindSemantics(d.cName, (*sample)[k])
			} else {
				possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
				t = "bpchar"
			}
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
		case "text","ntext":
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
			t = fmt.Sprintf("text")
			semantics = detectors.FindSemantics(d.cName, (*sample)[k])
		default:
			possibleActions = &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
			t = d.cTyp
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

func (da *SqlServer) resolveRefs(tip *types.TableInfoParams, ti *types.TableInfoData) error {

	rows, err := da.db.Query(`
	  SELECT
              obj.name AS [constraint_name],
	      col1.name AS [column_name],
              sch2.name AS [foreign_table_schema],
	      tab2.name AS [foreign_table_name],
	      col2.name AS [foreign_column_name]
	  FROM sys.foreign_key_columns fkc
	  INNER JOIN sys.objects obj
	      ON obj.object_id = fkc.constraint_object_id
	  INNER JOIN sys.tables tab1
	      ON tab1.object_id = fkc.parent_object_id
	  INNER JOIN sys.schemas sch
	      ON tab1.schema_id = sch.schema_id
	  INNER JOIN sys.columns col1
	      ON col1.column_id = parent_column_id AND col1.object_id = tab1.object_id
	  INNER JOIN sys.tables tab2
	      ON tab2.object_id = fkc.referenced_object_id
	  INNER JOIN sys.schemas sch2
	      ON tab2.schema_id = sch2.schema_id
	  INNER JOIN sys.columns col2
	      ON col2.column_id = referenced_column_id AND col2.object_id = tab2.object_id
          WHERE sch.name = @p1 and tab1.name = @p2`, tip.Schema, tip.Table)
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

func (da *SqlServer) isSysTable(tblName string) bool {
	return tblName == "MSreplication_options" ||
		tblName == "spt_fallback_db" ||
		tblName == "spt_fallback_dev" ||
		tblName == "spt_fallback_usg" ||
		tblName == "spt_monitor" ||
		tblName == "spt_values"
}

func (da *SqlServer) getSample(schema, table string, nColumns int) (*[][]string, error) {

	rows := make([][]*string, 0, detect.SampleSize)

	i := make([]interface{}, nColumns)
	s := make([]*string, nColumns)
	for k := 0; k != nColumns; k++ {
		i[k] = &s[k]
	}

	// sql := fmt.Sprintf(`SELECT * from [%s].[%s] TABLESAMPLE(%d ROWS)`, schema, table, detect.SampleSize)
	sql := fmt.Sprintf(`SELECT TOP %d * from [%s].[%s] ORDER BY NEWID()`, detect.SampleSize, schema, table)
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
