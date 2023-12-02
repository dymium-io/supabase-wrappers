module github.com/apex/log

go 1.20

replace github.com/tj/go-kinesis v0.0.0-00010101000000-000000000000 => ../go-kinesis

replace github.com/tj/go-elastic => ../go-elastic

//replace go-elastic/batch => ../go-elastic/batch

replace github.com/tj/go-elastic/batch v0.0.0-00010101000000-000000000000 => ../go-elastic/batch

require (
	github.com/aws/aws-sdk-go v1.48.5
	github.com/go-logfmt/logfmt v0.6.0
	github.com/pkg/errors v0.9.1
	github.com/rogpeppe/fastuuid v1.2.0
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9
	github.com/stretchr/testify v1.8.2
	github.com/tj/assert v0.0.3
	github.com/tj/go-kinesis v0.0.0-00010101000000-000000000000
)

//require github.com/tj/go-kinesis

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/smartystreets/assertions v1.13.1 // indirect
	github.com/smartystreets/gunit v1.4.5 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)
