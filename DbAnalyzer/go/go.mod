module DbAnalyzer

go 1.20

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/aws/aws-lambda-go v1.40.0
	github.com/aws/aws-sdk-go v1.44.255
	github.com/denisenkom/go-mssqldb v0.12.3
	github.com/go-sql-driver/mysql v1.7.1
	github.com/lib/pq v1.10.9
	github.com/sijms/go-ora/v2 v2.7.2
)

require (
	github.com/golang-sql/civil v0.0.0-20220223132316-b832511892a9 // indirect
	github.com/golang-sql/sqlexp v0.1.0 // indirect
	golang.org/x/crypto v0.8.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
)

replace aws => ../../libs/go/aws
