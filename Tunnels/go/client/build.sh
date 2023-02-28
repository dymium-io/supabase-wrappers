#!/bin/bash

go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=6' -X 'main.ProtocolVersion=6' -w -extldflags "-static"' -o dymium
cp ./dymium ../../../web/go/assets/customer/update/darwin/amd64/


