go build -a -tags netgo -ldflags "-X 'main.MajorVersion=0' -X 'main.MinorVersion=6' -X 'main.ProtocolVersion=6' -w -extldflags '-static'" -o dymium.exe
copy dymium.exe ..\..\..\web\go\assets\customer\update\windows\amd64\client
