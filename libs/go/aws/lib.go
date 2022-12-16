package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/lambda"
	"github.com/aws/aws-secretsmanager-caching-go/secretcache"

	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
)

var region string

var lambdas_map map[string]string

var ssm_map map[string]string
var secretCache *secretcache.Cache

func init() {
	lambdas, ok := os.LookupEnv("AWS_LAMBDAS")
	if ok {
		err := json.Unmarshal([]byte(lambdas), &lambdas_map)
		if err != nil {
			log.Printf("can not unmarshall `%s`", lambdas)
			os.Exit(-1)
		}
	} else {
		region, ok = os.LookupEnv("AWS_REGION")
		if !ok {
			log.Printf("AWS_REGION not defined")
			os.Exit(-1)
		}
	}

	ssm, ok := os.LookupEnv("AWS_SECRETS")
	if ok {
		err := json.Unmarshal([]byte(ssm), &ssm_map)
		if err != nil {
			log.Printf("can not unmarshall `%s`", ssm)
			os.Exit(-1)
		}
	} else {
		var err error
		secretCache, err = secretcache.New()
		if err != nil {
			log.Printf("Can not initialize secretCache")
			os.Exit(-1)
		}
	}
}

type E struct {
	ErrorMessage *string `json:"errorMessage"`
	ErrorType    *string `json:"errorType"`
}

func Invoke(fname string, qualifier *string, payload []byte) (r []byte, err error) {
	if lambdas_map == nil { // running in cloud!
		sess := session.Must(session.NewSessionWithOptions(session.Options{
			SharedConfigState: session.SharedConfigEnable,
		}))

		client := lambda.New(sess, &aws.Config{Region: aws.String(region)})
		var result *lambda.InvokeOutput
		if result, err = client.Invoke(
			&lambda.InvokeInput{
				FunctionName: aws.String(fname),
				Payload:      payload,
				Qualifier:    qualifier,
			}); err != nil {
			return nil, err
		}
		r = result.Payload
		var e E
		if json.Unmarshal(r, &e) == nil && e.ErrorMessage != nil {
			if e.ErrorType != nil {
				return nil, fmt.Errorf("%s: %s", *e.ErrorType, *e.ErrorMessage)
			} else {
				return nil, fmt.Errorf("%s", *e.ErrorMessage)
			}
		}
		return r, err
	} else {
		hostname, ok := lambdas_map[fname]
		if !ok {
			return nil, errors.New("function `" + fname + "` is not defined")
		}
		url := fmt.Sprintf("http://%s/2015-03-31/functions/function/invocations", hostname)
		reader := bytes.NewReader(payload)

		resp, err := http.Post(url, "application/json", reader)
		if err != nil {
			return nil, err
		}
		defer resp.Body.Close()
		if r, err = ioutil.ReadAll(resp.Body); err != nil {
			return nil, err
		}
		var e E
		if json.Unmarshal(r, &e) == nil && e.ErrorMessage != nil {
			if e.ErrorType != nil {
				return nil, fmt.Errorf("%s: %s", *e.ErrorType, *e.ErrorMessage)
			} else {
				return nil, fmt.Errorf("%s", *e.ErrorMessage)
			}
		}
		return r, err
	}
}

func GetSecret(secretId string) (v string, err error) {
	if ssm_map == nil {
		v, err = secretCache.GetSecretString(secretId)
		return v, err
	} else {
		var ok bool
		v, ok = ssm_map[secretId]
		if ok {
			return v, nil
		} else {
			return v, fmt.Errorf("Secret `%s` is not defined")
		}
	}
}
