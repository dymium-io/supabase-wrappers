module logcollector

go 1.20

require (
	github.com/aws/aws-sdk-go v1.44.260 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	github.com/smartystreets/assertions v1.13.1 // indirect
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9 // indirect
	github.com/smartystreets/gunit v1.4.5 // indirect
	github.com/tj/go-elastic v0.0.0-20171221160941-36157cbbebc2 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

require (
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
)

replace aws => ../../../libs/go/aws

//replace github.com/apex/log => ../../libs/go/log

replace github.com/tj/go-kinesis => ../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../libs/go/dlog

replace dymium.com/dymium/gotypes => ../../libs/go/gotypes

replace dymium.com/server/protocol => ../../Tunnels/go/protocol
