#!/bin/bash
CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
cp ./meshconnector ../../../web/go/assets/customer/connector/darwin/amd64/


CGO_ENABLED=0 GOOS=windows GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
cp ./meshconnector ../../../web/go/assets/customer/connector/windows/amd64/


CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
cp ./meshconnector ../../../web/go/assets/customer/connector/linux/amd64/
