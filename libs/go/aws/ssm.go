package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/secretsmanager"

	"fmt"
)

func GetSecret(secretName string) (string, error) {
	if ssm_map == nil {
		sess := session.Must(session.NewSessionWithOptions(session.Options{
			SharedConfigState: session.SharedConfigEnable,
		}))
		svc := secretsmanager.New(sess, &aws.Config{Region: aws.String(region)})
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
		} else {
			return *result.SecretString, nil
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
