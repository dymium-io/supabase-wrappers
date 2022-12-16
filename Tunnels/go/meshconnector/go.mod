module dymium.com/meshconnector

go 1.18

require (
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	dymium.com/meshconnector/ca v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
)

require (
	github.com/apex/log v1.9.0 // indirect
	github.com/aws/aws-sdk-go v1.20.6 // indirect
	github.com/gorilla/mux v1.8.0 // indirect
	github.com/jmespath/go-jmespath v0.0.0-20180206201540-c2b33e8439af // indirect
	github.com/jpillora/backoff v0.0.0-20180909062703-3050d21c67d7 // indirect
	github.com/pkg/errors v0.8.1 // indirect
	github.com/rogpeppe/fastuuid v1.1.0 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

replace dymium.com/meshconnector/ca => ../ca

replace dymium.com/server/protocol => ../protocol

replace dymium.com/dymium/log => ../../../libs/go/dlog
