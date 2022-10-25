

cd ..\..\go\client
go build -a -tags netgo -ldflags "-X ^"main.MajorVersion=0^" -X ^"main.MinorVersion=1^" -X ^"main.ProtocolVersion=4^" -extldflags ^"-static^"" -o dymium.exe
copy dymium.exe ..\..\installer\Windows
cd   ..\..\installer\Windows
