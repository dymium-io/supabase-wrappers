#!/bin/bash

go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1' -X 'main.ProtocolVersion=2' -w -extldflags "-static"' -o client
cp ./client ../../../web/go/assets/customer/update/darwin/amd64/


