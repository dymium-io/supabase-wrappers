package main

import (
	"github.com/aws/aws-lambda-go/lambda"

	"fmt"

	"DbAnalyzer/types"

	"database/sql"
)

const cacheN = 5
var nextCacheNo int
type conT struct {
	connectionParams types.ConnectionParams
	db *sql.DB
}
var cache [cacheN]*conT

func main() {
	lambda.Start(LambdaHandler)
}

func LambdaHandler(c types.AnalyzerRequest) (*types.AnalyzerResponse, error) {

	con, err := connect(&c.Connection)
	if err != nil {
		return nil, err
	}

	switch c.Dtype {
	case types.DT_Test:
		return &types.AnalyzerResponse{
			Dtype: types.DT_Test,
			DbInfo: nil,
			TblInfo: nil,
		}, nil
	case types.DT_DatabaseInfo:
		if di, err := getDbInfo(&c.Connection, con.db); err != nil {
			return nil, err
		} else {
			return &types.AnalyzerResponse{
				Dtype: types.DT_DatabaseInfo,
				DbInfo: di,
				TblInfo: nil,
			}, nil
		}
	case types.DT_TableInfo:
		if ti, err := getTblInfo(&c.Connection, c.TableInfo, con.db); err != nil {
			return nil, err
		} else {
			return &types.AnalyzerResponse{
				Dtype: types.DT_DatabaseInfo,
				DbInfo: nil,
				TblInfo: ti,
			}, nil
		}
		
	}
	
	return nil, nil
}

func connect(connectionParams *types.ConnectionParams) (*conT, error) {
	var con *conT
	for k := 0; k != nextCacheNo; k++ {
		con = cache[k]
		if con.connectionParams == *connectionParams {
			if con.db.Ping() != nil {
				if db, err := doConnect(connectionParams); err == nil {
					con.db = db
				} else {
					squeezeCache(k)
					return nil, err
				}
			}
			return con, nil
		}
	}
	if db, err := doConnect(connectionParams); err == nil {
		con = &conT{
			connectionParams: *connectionParams,
			db: db,
		}
	} else {
		return nil, err
	}
	if nextCacheNo == cacheN {
		cache[0].db.Close()
		squeezeCache(0)
	}
	cache[nextCacheNo] = con
	nextCacheNo++
	return con, nil
}

func squeezeCache(k int) {
	for kk := k+1; kk != nextCacheNo; kk++ {
		cache[k - 1] = cache[k]
	}
	nextCacheNo--
}

func doConnect(c *types.ConnectionParams) (*sql.DB, error) {
	switch c.Typ {
	case types.CT_PostgreSQL:
		return connectPostgres(c)
	case types.CT_MySQL, types.CT_MariaDB:
		return connectMysql(c)
	case types.CT_SqlServer:
		return connectTds(c)
	case types.CT_OracleDB:
		return connectOra(c)
	}
	return nil, fmt.Errorf("Data sources of type %v are not supported yet", c.Typ)
}

func getDbInfo(c *types.ConnectionParams, db *sql.DB) (*types.DatabaseInfo, error) {
	switch c.Typ {
	case types.CT_PostgreSQL:
		return getPostgresInfo(c.Database, db)
	case types.CT_MySQL, types.CT_MariaDB:
		return getMysqlInfo(c.Database, db)
	case types.CT_SqlServer:
		return getTdsInfo(c.Database, db)
	case types.CT_OracleDB:
		return getOraInfo(c.Database, db)
	}
	return nil, fmt.Errorf("Data sources of type %v are not supported yet", c.Typ)
}

func getTblInfo(c *types.ConnectionParams, t *types.TableInfoParams, db *sql.DB) (*types.TableInfo, error) {
	switch c.Typ {
	case types.CT_PostgreSQL:
		return getPostgresTblInfo(c.Database, t, db)
	/*
	case types.CT_MySQL, types.CT_MariaDB:
		return getMysqlInfo(c.Database, db)
	case types.CT_SqlServer:
		return getTdsInfo(c.Database, db)
	case types.CT_OracleDB:
		return getOraInfo(c.Database, db)
	*/
	}
	return nil, fmt.Errorf("Data sources of type %v are not supported yet", c.Typ)
}
