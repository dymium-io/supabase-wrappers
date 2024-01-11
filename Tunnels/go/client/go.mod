module dymium.com/client

go 1.20

require (
	github.com/fatih/color v1.16.0 // indirect
	github.com/mattn/go-colorable v0.1.13 // indirect
	github.com/mattn/go-isatty v0.0.20 // indirect
	github.com/pkg/errors v0.9.1 // indirect

)

replace dymium.com/server/protocol => ../protocol

replace dymium.com/client/ca => ../ca

replace dymium.com/client/types => ./types

require (
	dymium.com/client/ca v0.0.0-00010101000000-000000000000
	dymium.com/client/types v0.0.0-00010101000000-000000000000
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
	github.com/blang/semver/v4 v4.0.0
	github.com/golang-jwt/jwt v3.2.2+incompatible
	github.com/gorilla/mux v1.8.1
	github.com/pkg/browser v0.0.0-20210911075715-681adbf594b8
	golang.org/x/sys v0.15.0
)
