package main

import (
	"github.com/aws/aws-lambda-go/lambda"

	"fmt"

	"DbAnalyzer/types"
	"DbAnalyzer/utils"

	"database/sql"
)

type conT struct {
	connectionParams types.ConnectionParams
	db               *sql.DB
}

func (c *conT) IsTheSame(el any) bool {
	return c.connectionParams == *el.(*types.ConnectionParams)
}

var cache *utils.Cache[*conT]

func init() {
	cache = utils.MakeCache[*conT](5)
}


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
			Dtype:   types.DT_Test,
			DbInfo:  nil,
			TblInfo: nil,
		}, nil
	case types.DT_DatabaseInfo:
		if di, err := getDbInfo(&c.Connection, con.db); err != nil {
			return nil, err
		} else {
			return &types.AnalyzerResponse{
				Dtype:   types.DT_DatabaseInfo,
				DbInfo:  di,
				TblInfo: nil,
			}, nil
		}
	case types.DT_TableInfo:
		if ti, err := getTblInfo(&c.Connection, c.TableInfo, con.db); err != nil {
			return nil, err
		} else {
			return &types.AnalyzerResponse{
				Dtype:   types.DT_DatabaseInfo,
				DbInfo:  nil,
				TblInfo: ti,
			}, nil
		}

	}

	return nil, nil
}

func connect(connectionParams *types.ConnectionParams) (*conT, error) {
	con := cache.Find(connectionParams)

	if con != nil {
		if err := con.db.Ping(); err != nil {
			cache.Pop().db.Close()
			return nil, err
		} else {
			return con, nil
		}
	}

	if db, err := doConnect(connectionParams); err != nil {
		return nil, err
	} else {
		con = &conT{
			connectionParams: *connectionParams,
			db:               db,
		}
		if old := cache.Push(con); old != nil {
			old.db.Close()
		}
		return con, nil
	}
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

func getDbInfo(c *types.ConnectionParams, db *sql.DB) (*types.DatabaseInfoData, error) {
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

func getTblInfo(c *types.ConnectionParams, t *types.TableInfoParams, db *sql.DB) (*types.TableInfoData, error) {
	switch c.Typ {
	case types.CT_PostgreSQL:
		return getPostgresTblInfo(c.Database, t, db)
	case types.CT_MySQL, types.CT_MariaDB:
		return getMysqlTblInfo(c.Database, t, db)
	case types.CT_SqlServer:
		return getTdsTblInfo(c.Database, t, db)
	case types.CT_OracleDB:
		return getOraTblInfo(c.Database, t, db)
	}
	return nil, fmt.Errorf("Data sources of type %v are not supported yet", c.Typ)
}
