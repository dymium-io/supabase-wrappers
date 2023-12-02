module dymium.com/dymium

require (
	aws v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/Jeffail/gabs v1.4.0
	github.com/andybalholm/brotli v1.0.6
	github.com/go-http-utils/etag v0.0.0-20161124023236-513ea8f21eb1
	github.com/golang-jwt/jwt v3.2.2+incompatible
	github.com/gorilla/mux v1.8.1
	github.com/lib/pq v1.10.9
	github.com/redis/go-redis/v9 v9.3.0
	github.com/stretchr/testify v1.8.2
	github.com/victorspringer/http-cache v0.0.0-20231006141456-6446fe59efba
	golang.org/x/exp v0.0.0-20231127185646-65229373498e
	golang.org/x/net v0.19.0
)

require (
	github.com/aws/aws-sdk-go v1.48.6 // indirect
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/go-http-utils/fresh v0.0.0-20161124030543-7231e26a4b27 // indirect
	github.com/go-http-utils/headers v0.0.0-20181008091004-fed159eddc2a // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

require (
	dymium.com/dymium/gotypes v0.0.0-00010101000000-000000000000
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0 // indirect
)

require (
	github.com/cespare/xxhash/v2 v2.2.0 // indirect
	github.com/dgryski/go-rendezvous v0.0.0-20200823014737-9f7001d12a5f // indirect
	github.com/kr/pretty v0.3.1 // indirect
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
)

replace aws => ../../../libs/go/aws

replace github.com/apex/log => ../../../libs/go/log

replace github.com/tj/go-kinesis => ../../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../../libs/go/dlog

replace dymium.com/dymium/gotypes => ../../../libs/go/gotypes

replace dymium.com/server/protocol => ../../../Tunnels/go/protocol

go 1.20
