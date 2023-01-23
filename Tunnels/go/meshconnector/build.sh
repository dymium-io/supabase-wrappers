#!/bin/bash

go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1'  -w -extldflags "-static"' -o /tmp/meshconnector
tar -C /tmp -cvzf  /tmp/meshconnector.tar.gz meshconnector
cp /tmp/meshconnector.tar.gz ../../../web/go/assets/customer//meshconnector_darwin_amd64.tar.gz
#aws s3  --profile dymium --region us-west-2 cp /tmp/meshconnector.tar.gz s3://dymium-connectors/macos/
rm /tmp/meshconnector.tar.gz


CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
	      go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1'  -w -extldflags "-static"' -o /tmp/meshconnector
tar -C /tmp -cvzf  /tmp/meshconnector.tar.gz meshconnector
cp /tmp/meshconnector.tar.gz ../../../web/go/assets/customer/meshconnector_linux_amd64.tar.gz
#aws s3  --profile dymium --region us-west-2 cp /tmp/meshconnector.tar.gz s3://dymium-connectors/linux/
rm /tmp/meshconnector.tar.gz

CGO_ENABLED=0 GOOS=windows GOARCH=amd64 \
	      go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1'  -w -extldflags "-static"' -o /tmp/meshconnector.exe
zip  /tmp/meshconnector.zip /tmp/meshconnector.exe
cp /tmp/meshconnector.zip ../../../web/go/assets/customer/meshconnector_windows_amd64.zip
#aws s3  --profile dymium --region us-west-2 cp /tmp/meshconnector.zip s3://dymium-connectors/windows/
rm /tmp/meshconnector.zip

