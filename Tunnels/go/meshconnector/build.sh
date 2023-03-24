#!/bin/bash
CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
codesign -dvv --timestamp -s "Developer ID Application: Dymium Inc (RC7F4R4R28)" --options=hard,expires,runtime ./meshconnector

aws s3  --profile dymium --region us-west-2 cp meshconnector  s3://dymium-connector/darwin/amd64




CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector

aws s3  --profile dymium --region us-west-2 cp meshconnector  s3://dymium-connector/linux/amd64
