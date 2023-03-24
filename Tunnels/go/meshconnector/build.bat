set CGO_ENABLED=0& set GOOS=windows& set GOARCH=amd64& 
go build -a -tags netgo -ldflags "-w -extldflags '-static'" -o meshconnector.exe

