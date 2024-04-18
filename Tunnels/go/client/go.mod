module dymium.com/client

go 1.20

require github.com/pkg/errors v0.9.1 // indirect

replace dymium.com/server/protocol => ../protocol

replace dymium.com/client/ca => ../ca

replace dymium.com/client/types => ./types

replace github.com/apex/log => ../../../libs/go/log

require (
	dymium.com/client/ca v0.0.0-00010101000000-000000000000
	dymium.com/client/types v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
	github.com/blang/semver/v4 v4.0.0
	github.com/golang-jwt/jwt v3.2.2+incompatible
	github.com/gorilla/mux v1.8.1
	github.com/pkg/browser v0.0.0-20240102092130-5ac0b6a4141c
	golang.org/x/sys v0.19.0
)
