module dymium.com/server

replace dymium.com/server/protocol => ../protocol

replace dymium.com/dymium/log => ../../../libs/go/dlog

replace github.com/apex/log => ../../../libs/go/log

replace github.com/tj/go-kinesis => ../../../libs/go/go-kinesis

replace dymium.com/server/gotypes => ../../../libs/go/gotypes

go 1.18

require (
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	dymium.com/server/gotypes v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/gorilla/mux v1.8.0
	github.com/redis/go-redis/v9 v9.0.4
)

require (
	github.com/apex/log v1.9.0 // indirect
	github.com/aws/aws-sdk-go v1.44.262 // indirect
	github.com/cespare/xxhash/v2 v2.2.0 // indirect
	github.com/dgryski/go-rendezvous v0.0.0-20200823014737-9f7001d12a5f // indirect
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
