module dymium.com/meshconnector

go 1.18

require (
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	dymium.com/meshconnector/ca v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/blang/semver/v4 v4.0.0
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/gorilla/mux v1.8.0
)

require (
	github.com/apex/log v1.9.0 // indirect
	github.com/aws/aws-sdk-go v1.44.254 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

replace dymium.com/meshconnector/ca => ../ca

replace dymium.com/server/protocol => ../protocol

replace dymium.com/dymium/log => ../../../libs/go/dlog
