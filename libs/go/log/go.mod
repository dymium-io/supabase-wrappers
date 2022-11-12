module github.com/apex/log

go 1.18

require (
	github.com/apex/logs v1.1.0
	github.com/aphistic/golf v0.0.0-20180712155816-02c07f170c5a
	github.com/aws/aws-sdk-go v1.44.130
	github.com/aybabtme/rgbterm v0.0.0-20170906152045-cc83f3b3ce59
	github.com/fatih/color v1.7.0
	github.com/go-logfmt/logfmt v0.4.0
	github.com/mattn/go-colorable v0.1.2
	github.com/pkg/errors v0.9.1
	github.com/rogpeppe/fastuuid v1.1.0
	github.com/stretchr/testify v1.6.1
	github.com/tj/assert v0.0.3
	github.com/tj/go-buffer v1.1.0
	github.com/tj/go-elastic v0.0.0-20171221160941-36157cbbebc2
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b
	github.com/tj/go-spin v1.1.0
)

require (
	github.com/aphistic/sweet v0.3.0 // indirect
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/google/uuid v1.3.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/kr/logfmt v0.0.0-20140226030751-b84e30acd515 // indirect
	github.com/mattn/go-isatty v0.0.8 // indirect
	github.com/onsi/gomega v1.24.0 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9 // indirect
	golang.org/x/net v0.1.0 // indirect
	golang.org/x/sys v0.1.0 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

replace github.com/tj/go-kinesis => ../go-kinesis
