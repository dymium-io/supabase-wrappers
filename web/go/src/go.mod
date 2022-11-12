module dymium.com/dymium

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/Jeffail/gabs v1.4.0
	github.com/andybalholm/brotli v1.0.4
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/go-http-utils/etag v0.0.0-20161124023236-513ea8f21eb1
	github.com/gorilla/mux v1.8.0
	github.com/lib/pq v1.10.7
	github.com/stretchr/testify v1.6.1
	github.com/victorspringer/http-cache v0.0.0-20221006212759-e323d9f0f0c4
	golang.org/x/net v0.1.0
)

require (
	github.com/aws/aws-sdk-go v1.44.130 // indirect
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

require github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect

replace aws => ../../../libs/go/aws

replace github.com/apex/log => ../../../libs/go/log

replace github.com/tj/go-kinesis => ../../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../../libs/go/dlog

replace dymium.com/dymium/gotypes => ../../../libs/go/gotypes

go 1.18
