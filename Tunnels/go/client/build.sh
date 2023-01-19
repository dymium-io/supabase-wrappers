#!/bin/bash

go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1' -X 'main.ProtocolVersion=5' -w -extldflags "-static"' -o dymium
cp ./dymium ../../../web/go/assets/customer/update/darwin/amd64/


