package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/lambda"
	"github.com/aws/aws-sdk-go/service/secretsmanager"

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

func GetSecret(secretName string) (string, error) {
	if ssm_map == nil {
		svc := secretsmanager.New(session.New())
		input := &secretsmanager.GetSecretValueInput{
			SecretId: aws.String(secretName),
		}

		result, err := svc.GetSecretValue(input)
		if err != nil {
			if aerr, ok := err.(awserr.Error); ok {
				switch aerr.Code() {
				case secretsmanager.ErrCodeResourceNotFoundException:
					return "", fmt.Errorf("Secret `%s`: %v: %v",
						secretName,
						secretsmanager.ErrCodeResourceNotFoundException,
						aerr.Error())
				case secretsmanager.ErrCodeInvalidParameterException:
					return "", fmt.Errorf("Secret `%s`: %v: %v",
						secretName,
						secretsmanager.ErrCodeInvalidParameterException,
						aerr.Error())
				case secretsmanager.ErrCodeInvalidRequestException:
					return "", fmt.Errorf("Secret `%s`: %v: %v",
						secretName,
						secretsmanager.ErrCodeInvalidRequestException,
						aerr.Error())
				case secretsmanager.ErrCodeDecryptionFailure:
					return "", fmt.Errorf("Secret `%s`: %v: %v",
						secretName,
						secretsmanager.ErrCodeDecryptionFailure,
						aerr.Error())
				case secretsmanager.ErrCodeInternalServiceError:
					return "", fmt.Errorf("Secret `%s`: %v: %v",
						secretName,
						secretsmanager.ErrCodeInternalServiceError,
						aerr.Error())
				default:
					return "", fmt.Errorf("Secret `%s`: %v",
						secretName,
						aerr.Error())
				}
			} else {
				// Print the error, cast err to awserr.Error to get the Code and
				// Message from an error.
				return "", fmt.Errorf("Secret `%s`: %v",
					secretName,
					aerr.Error())
			}
			return result.GoString(), nil
		}
	} else {
		var ok bool
		var v string
		v, ok = ssm_map[secretName]
		if ok {
			return v, nil
		} else {
			return v, fmt.Errorf("Secret `%s` is not defined", secretName)
		}
	}
	return "", fmt.Errorf("Secret `%s`: impossible", secretName)
}
