module dymium.com/client

go 1.18

require (
	github.com/fatih/color v1.7.0 // indirect
	github.com/mattn/go-colorable v0.1.2 // indirect
	github.com/mattn/go-isatty v0.0.8 // indirect
	github.com/pkg/errors v0.8.1 // indirect
	golang.org/x/sys v0.0.0-20210616045830-e2b7044e8c71 // indirect

)

replace dymium.com/server/protocol => ../protocol

require (
	dymium.com/server/protocol v0.0.0-00010101000000-000000000000
	github.com/apex/log v1.9.0
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/gorilla/mux v1.8.0
	github.com/pkg/browser v0.0.0-20210911075715-681adbf594b8
)
