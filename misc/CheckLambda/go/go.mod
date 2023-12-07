module CheckLambda

go 1.18

replace aws => ../../../libs/go/aws

require aws v0.0.0-00010101000000-000000000000

require (
	github.com/aws/aws-sdk-go v1.48.5 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
)
