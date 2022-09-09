module dymium.com/client

go 1.18

require (
	github.com/Jeffail/gabs v1.4.0 // indirect
	github.com/gorilla/mux v1.8.0 // indirect
	github.com/kardianos/osext v0.0.0-20190222173326-2bc1f35cddc0 // indirect
	github.com/kr/binarydist v0.1.0 // indirect
	github.com/pkg/browser v0.0.0-20210911075715-681adbf594b8 // indirect
	golang.org/x/crypto v0.0.0-20211209193657-4570a0811e8b // indirect
	golang.org/x/sys v0.0.0-20210616045830-e2b7044e8c71 // indirect

)

replace dymium.com/server/protocol => ../protocol

require dymium.com/server/protocol v0.0.0-00010101000000-000000000000
