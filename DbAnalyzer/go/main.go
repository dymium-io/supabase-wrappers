package main

import (
	"dymium.com/dymium/log"
	"github.com/aws/aws-lambda-go/lambda"
	"os"
	"strconv"

	"DbAnalyzer/types"
	"DbAnalyzer/utils"
)

func dbAnalyzer(dt types.ConnectionType) DA {
	var da DA
	switch dt {
	case types.CT_PostgreSQL:
		da = &Postgres{}
	case types.CT_MySQL:
		da = &MySQL{}
	case types.CT_MariaDB:
		da = &MySQL{}
	case types.CT_SqlServer:
		da = &SqlServer{}
	case types.CT_OracleDB:
		da = &OracleDB{}
	case types.CT_DB2:
		da = &DB2{}
	case types.CT_MongoDB:
		da = &MongoClient{}
	case types.CT_Elasticsearch:
		host := os.Getenv("DASVC_HOST")
		if host == "" {
			host = "localhost"
		}

		port := 8888
		portStr := os.Getenv("DASVC_PORT")
		if portStr != "" {
			p, err := strconv.Atoi(portStr)
			if err != nil {
				log.Errorf("Error converting %s to int: %v, using default value.\n", portStr, err)
			} else {
				port = p
			}
		}

		da = &JdbcClient{
			KtDAHost: host,
			KtDAPort: port,
		}
	}
	return da
}

type conT struct {
	connectionParams types.ConnectionParams
	da               DA
}

func (c *conT) IsTheSame(el any) bool {
	return c.connectionParams == *el.(*types.ConnectionParams)
}

var cache *utils.Cache[*conT]

func init() {
	cache = utils.MakeCache[*conT](5)
}

func main() {
	log.Init("DbAnalyzer")
	lambda.Start(LambdaHandler)
}

type DA interface {
	Connect(*types.ConnectionParams) error
	GetDbInfo(string) (*types.DatabaseInfoData, error)
	GetTblInfo(string, *types.TableInfoParams) (*types.TableInfoData, error)
	Ping() error
	Close()
}

func LambdaHandler(c types.AnalyzerRequest) (*types.AnalyzerResponse, error) {

	da, err := connect(&c.Connection)
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
		if di, err := da.GetDbInfo(c.Connection.Database); err != nil {
			return nil, err
		} else {
			return &types.AnalyzerResponse{
				Dtype:   types.DT_DatabaseInfo,
				DbInfo:  di,
				TblInfo: nil,
			}, nil
		}
	case types.DT_TableInfo:
		if ti, err := da.GetTblInfo(c.Connection.Database, c.TableInfo); err != nil {
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

func connect(connectionParams *types.ConnectionParams) (DA, error) {
	con := cache.Find(connectionParams)

	if con != nil {
		if err := con.da.Ping(); err != nil {
			cache.Pop().da.Close()
			return nil, err
		} else {
			return con.da, nil
		}
	}

	da := dbAnalyzer(connectionParams.Typ)
	if err := da.Connect(connectionParams); err != nil {
		return da, err
	} else {
		con = &conT{
			connectionParams: *connectionParams,
			da:               da,
		}
		if old := cache.Push(con); old != nil {
			old.da.Close()
		}
		return con.da, nil
	}
}
