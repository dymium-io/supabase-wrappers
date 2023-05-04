package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/lambda"

	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
)

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
