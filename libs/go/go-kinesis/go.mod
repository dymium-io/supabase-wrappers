module go-kinesis

go 1.18

require (
	github.com/apex/log v1.9.0
	github.com/aws/aws-sdk-go v1.44.254
	github.com/jpillora/backoff v1.0.0
)

require (
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
)

replace github.com/apex/log => ../../../libs/go/log
