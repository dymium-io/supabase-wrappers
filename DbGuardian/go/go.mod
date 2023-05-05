module initializer

go 1.18

replace aws => ../../libs/go/aws

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/lib/pq v1.10.9
)

require (
	github.com/aws/aws-sdk-go v1.44.255 // indirect
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
)
