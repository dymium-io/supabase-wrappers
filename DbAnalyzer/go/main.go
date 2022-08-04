package main

import (
	"github.com/aws/aws-lambda-go/lambda"
	
	"fmt"
	
	"DbAnalyzer/types"
)


func LambdaHandler(c types.Connection) (interface{}, error) {

	switch c.Typ {
	case types.CT_PostgreSQL: return getPostgresInfo(c)
	case types.CT_MySQL: return getMysqlInfo(c)
	}
	return nil, fmt.Errorf("Data sources of type %v are not supported yet",c.Typ)
}


func main() {
        lambda.Start(LambdaHandler)
}
