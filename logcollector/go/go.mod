module logsupervisor

go 1.20

require github.com/looplab/fsm v1.0.1

require (
	github.com/aws/aws-sdk-go v1.44.130 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/rogpeppe/fastuuid v1.1.0 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

require (
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
)

replace aws => ../../../libs/go/aws

replace github.com/apex/log => ../../libs/go/log

replace github.com/tj/go-kinesis => ../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../libs/go/dlog

replace dymium.com/dymium/gotypes => ../../libs/go/gotypes

replace dymium.com/server/protocol => ../../Tunnels/go/protocol
