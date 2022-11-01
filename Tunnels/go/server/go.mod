module dymium.com/server

replace dymium.com/server/protocol => ../protocol

go 1.18

require dymium.com/server/protocol v0.0.0-00010101000000-000000000000

require (
	github.com/gorilla/mux v1.8.0 // indirect
	github.com/lib/pq v1.10.7 // indirect
)
