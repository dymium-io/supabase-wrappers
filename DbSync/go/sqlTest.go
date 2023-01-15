package main

import (
	"database/sql"
	_ "github.com/lib/pq"

	"fmt"
	"log"

	"crypto/md5"
	b64 "encoding/base64"

	"DbSync/types"
)

func sqlTest(
	datascope *string,
	sqlTest *types.SqlTestConf,
	cnf *guardianConf,
) (*types.SqlTestResult, error) {

	if sqlTest == nil {
		return nil, fmt.Errorf("sqlTestConf not defined")
	}

	log.Printf("sqlTest: { datascope: %s, database: %s, schema: %s, table: %s }\n",
		*datascope, sqlTest.Database, sqlTest.Schema, sqlTest.Table)
	log.Printf("guardianCnf: { Address: %v:%d, Database: %s}\n",
		cnf.GuardianAddress, cnf.GuardianPort, cnf.GuardianDatabase)

	sslmode_ := "disable"
	if *cnf.GuardianTls {
		sslmode_ = "require"
	}

	if len(cnf.GuardianAddress) == 0 {
		return nil, fmt.Errorf("Can not connect to the Guardian DB: address not defined")
	}

	connectStr := fmt.Sprintf("host=%s port=%d dbname='%s' user=%s password='%s' sslmode=%s",
		cnf.GuardianAddress[0], cnf.GuardianPort, *datascope, cnf.GuardianUser, cnf.GuardianAdminPassword, sslmode_)

	var err error
	var db *sql.DB

	if db, err = sql.Open("postgres", connectStr); err != nil {
		return nil, fmt.Errorf("Cannot open connection to %s. Ignoring error: %v", cnf.GuardianAddress[0], err)
	}
	defer db.Close()

	if _, err := db.Exec("SET SESSION AUTHORIZATION " + fmt.Sprintf("_%x_", md5.Sum([]byte(*datascope+"_dymium")))); err != nil {
		return nil, fmt.Errorf("session authorization failed: %v", err)
	}

	var rows *sql.Rows
	sql := fmt.Sprintf(`select * from %s_%s.%s limit 20`,
		sqlTest.Database, sqlTest.Schema, sqlTest.Table)
	if rows, err = db.Query(sql); err != nil {
		return nil, fmt.Errorf("Query [%s] failed: %v", sql, err)
	}
	defer rows.Close()

	var columns []string
	if columns, err = rows.Columns(); err != nil {
		return nil, fmt.Errorf("Can not get list of columns in the result of [%s]: %v", sql, err)
	}
	columnTypes := make([]string, len(columns))
	{
		if cts, err := rows.ColumnTypes(); err != nil {
			return nil, fmt.Errorf("Can not get list of column types in the result of [%s]: %v", sql, err)
		} else {
			for k, ct := range cts {
				columnTypes[k] = ct.ScanType().String()
			}
		}
	}
	result := types.SqlTestResult{
		Columns: columns,
		Records: make([][]string, 0, 20),
	}

	iCols := make([]interface{}, len(columns))
	sCols := make([]*string, len(columns))
	bCols := make([]*[]byte, len(columns))
	for k, ct := range columnTypes {
		if ct == "[]uint8" {
			iCols[k] = &bCols[k]
		} else {
			iCols[k] = &sCols[k]
		}
	}
	for rows.Next() {
		if err = rows.Scan(iCols...); err != nil {
			return nil, fmt.Errorf("Scan error in [%s]: %v", sql, err)
		}
		rCols := make([]string, len(columns))
		for k, ct := range columnTypes {
			if ct == "[]uint8" {
				if bCols[k] == nil {
					rCols[k] = "0NULL"
				} else {
					rCols[k] = "1" + b64.StdEncoding.EncodeToString(*bCols[k])
				}
			} else {
				if sCols[k] == nil {
					rCols[k] = "0NULL"
				} else {
					rCols[k] = "0" + *sCols[k]
				}
			}
		}
		result.Records = append(result.Records, rCols)
	}

	return &result, nil
}
