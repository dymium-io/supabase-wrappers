module initializer

go 1.18

replace aws => ../../libs/go/aws

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/aclements/go-gg v0.0.0-20170323211221-abd1f791f5ee
	github.com/aws/aws-lambda-go v1.32.0
	github.com/lib/pq v1.10.6
)

require (
	github.com/aws/aws-sdk-go v1.44.27 // indirect
	github.com/aws/aws-secretsmanager-caching-go v1.1.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
)
