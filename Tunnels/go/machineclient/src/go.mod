module dymium.com/machineclient

go 1.20

require (
	dymium.com/client/ca v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
)

require (
	dymium.com/client/sockopts v0.0.0-00010101000000-000000000000
	dymium.com/client/types v0.0.0-00010101000000-000000000000
	dymium.com/meshconnector/selfupdate v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
	github.com/blang/semver/v4 v4.0.0
)

require (
	github.com/golang-jwt/jwt v3.2.2+incompatible // indirect
	github.com/gorilla/mux v1.8.1
	github.com/pkg/errors v0.9.1 // indirect
	golang.org/x/sys v0.15.0 // indirect
)

replace dymium.com/server/protocol => ../../protocol

replace dymium.com/client/ca => ../../ca

replace dymium.com/client/sockopts => ../../sockopts

replace dymium.com/client/types => ../../client/types

replace github.com/apex/log => ../../../../libs/go/log

replace dymium.com/meshconnector/selfupdate => ../../selfupdate
