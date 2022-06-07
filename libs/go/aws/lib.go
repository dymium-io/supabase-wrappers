package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/lambda"

	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"encoding/json"
	"log"
	"net/http"
	"os"
)

var fnmap map[string]string
var region string

func init() {
	lambdas, ok := os.LookupEnv("AWS_LAMBDAS")
	if ok {
		err := json.Unmarshal([]byte(lambdas), &fnmap)
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
}

func Invoke(fname string, qualifier *string, payload []byte) (r []byte, err error) {
	if fnmap == nil { // running in cloud!
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
		return r, err
	} else {
		hostname, ok := fnmap[fname]
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
		r, err = ioutil.ReadAll(resp.Body)
		return r, err
	}
}
