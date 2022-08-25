module dymium.com/client

go 1.18

require (
	github.com/Jeffail/gabs v1.4.0 // indirect
	github.com/gorilla/mux v1.8.0 // indirect
	github.com/pkg/browser v0.0.0-20210911075715-681adbf594b8 // indirect
	golang.org/x/sys v0.0.0-20210616045830-e2b7044e8c71 // indirect
)
replace dymium.com/server/protocol => ../protocol
require dymium.com/server/protocol v0.0.0-00010101000000-000000000000