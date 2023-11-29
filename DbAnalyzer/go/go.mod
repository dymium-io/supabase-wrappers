module DbAnalyzer

go 1.20

require (
	aws v0.0.0-00010101000000-000000000000
	dymium.com/dymium/log v0.0.0-00010101000000-000000000000
	github.com/aws/aws-lambda-go v1.41.0
	github.com/denisenkom/go-mssqldb v0.12.3
	github.com/go-sql-driver/mysql v1.7.1
	github.com/ibmdb/go_ibm_db v0.4.5
	github.com/jmoiron/sqlx v1.3.5
	github.com/lib/pq v1.10.9
	github.com/sijms/go-ora/v2 v2.7.23
	go.mongodb.org/mongo-driver v1.13.0
	golang.org/x/exp v0.0.0-20231127185646-65229373498e
)

require (
	github.com/apex/log v1.9.0 // indirect
	github.com/aws/aws-sdk-go v1.48.8 // indirect
	github.com/golang-sql/civil v0.0.0-20220223132316-b832511892a9 // indirect
	github.com/golang-sql/sqlexp v0.1.0 // indirect
	github.com/golang/snappy v0.0.4 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jpillora/backoff v1.0.0 // indirect
	github.com/klauspost/compress v1.17.3 // indirect
	github.com/montanaflynn/stats v0.7.1 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	github.com/smartystreets/go-aws-auth v0.0.0-20180515143844-0c1422d1fdb9 // indirect
	github.com/tj/go-kinesis v0.0.0-20171128231115-08b17f58cb1b // indirect
	github.com/xdg-go/pbkdf2 v1.0.0 // indirect
	github.com/xdg-go/scram v1.1.2 // indirect
	github.com/xdg-go/stringprep v1.0.4 // indirect
	github.com/youmark/pkcs8 v0.0.0-20201027041543-1326539a0a0a // indirect
	golang.org/x/crypto v0.16.0 // indirect
	golang.org/x/sync v0.5.0 // indirect
	golang.org/x/text v0.14.0 // indirect
)

replace aws => ../../libs/go/aws

replace github.com/apex/log => ../../libs/go/log

replace github.com/tj/go-kinesis => ../../libs/go/go-kinesis

replace dymium.com/dymium/log => ../../libs/go/dlog
