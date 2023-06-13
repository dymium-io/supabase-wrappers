module DbSync

go 1.18

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/aclements/go-gg v0.0.0-20170323211221-abd1f791f5ee
	github.com/aws/aws-lambda-go v1.40.0
	github.com/lib/pq v1.10.9
	golang.org/x/exp v0.0.0-20230425010034-47ecfdc1ba53
)

require (
	github.com/aws/aws-sdk-go v1.44.260 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
)

replace aws => ../../libs/go/aws
