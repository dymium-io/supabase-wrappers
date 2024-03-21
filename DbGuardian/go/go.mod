module initializer

go 1.20

replace aws => ../../libs/go/aws

replace DbSetup => ../../libs/go/DbSetup

require (
	aws v0.0.0-00010101000000-000000000000
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	github.com/lib/pq v1.10.9
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

require DbSetup v0.0.0-00010101000000-000000000000

require (
	github.com/apex/log v1.9.0 // indirect
	github.com/aws/aws-sdk-go v1.48.8 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9 // indirect
)

replace github.com/apex/log => ../../libs/go/log

replace github.com/tj/go-kinesis => ../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../libs/go/dlog
