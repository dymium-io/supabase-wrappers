package aws

import (
	"encoding/json"
	"log"
	"os"
)

var region string

var lambdas_map map[string]string

var ssm_map map[string]string

func init() {

	var ok bool
	var lambdas, ssm string
	region, ok = os.LookupEnv("AWS_REGION")
	if ok {
		// We are in the Cloud!
		// no further initialization needed
		return
	}

	lambdas, ok = os.LookupEnv("AWS_LAMBDAS")
	if ok {
		err := json.Unmarshal([]byte(lambdas), &lambdas_map)
		if err != nil {
			log.Printf("can not unmarshall `%s`", lambdas)
			os.Exit(-1)
		}
	}

	ssm, ok = os.LookupEnv("AWS_SECRETS")
	if ok {
		err := json.Unmarshal([]byte(ssm), &ssm_map)
		if err != nil {
			log.Printf("can not unmarshall `%s`", ssm)
			os.Exit(-1)
		}
	}
}

type E struct {
	ErrorMessage *string `json:"errorMessage"`
	ErrorType    *string `json:"errorType"`
}
