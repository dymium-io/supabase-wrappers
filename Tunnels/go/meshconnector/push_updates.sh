#!/bin/bash


# This code is used to prevent the script from continuing if a command fails.
# This is useful because it prevents the script from continuing after a command
# fails, which can cause unexpected behavior.
# This code also makes it easier to see which commands fail.
set -e

# make the script output its commands before executing them
set -x

# Default to 'dev' if no argument is provided
ENVIRONMENT=${1:-dev}
PROFILE=dymium
# Determine the S3 bucket based on the environment
case $ENVIRONMENT in
  dev)
    S3_BUCKET="dymium-dev-connectors"
    PROFILE="dymium-dev"
    ;;
  prod)
    S3_BUCKET="dymium-prod-connectors"
    PROFILE="dymium-prod"
    ;;
  stage)
    S3_BUCKET="dymium-stage-connectors"
    PROFILE="dymium-stage"
    ;;
  *)
    echo "Invalid environment. Please specify 'dev', 'prod', or 'stage'."
    exit 1
    ;;
esac

CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
chmod a+x meshconnector    
codesign -dvv --timestamp -s "Developer ID Application: Dymium Inc (RC7F4R4R28)" --options=hard,expires,runtime ./meshconnector
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector  s3://$S3_BUCKET/darwin/amd64/meshconnector
tar cvzf meshconnector_darwin_amd64.tgz -C ./ meshconnector
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector_darwin_amd64.tgz  s3://$S3_BUCKET/darwin/meshconnector_darwin_amd64.tgz
rm meshconnector_darwin_amd64.tgz


CGO_ENABLED=0 GOOS=darwin GOARCH=arm64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
chmod a+x meshconnector     
codesign -dvv --timestamp -s "Developer ID Application: Dymium Inc (RC7F4R4R28)" --options=hard,expires,runtime ./meshconnector
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector  s3://$S3_BUCKET/darwin/arm64/meshconnector


CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o meshconnector
chmod a+x meshconnector     
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector  s3://$S3_BUCKET/linux/amd64/meshconnector
tar cvzf meshconnector_linux_amd64.tgz -C ./ meshconnector
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector_linux_amd64.tgz  s3://$S3_BUCKET/linux/meshconnector_linux_amd64.tgz


CGO_ENABLED=0 GOOS=windows GOARCH=amd64 \
go build -a -tags netgo -ldflags "-w -extldflags '-static'" -o meshconnector.exe
chmod a+x meshconnector.exe
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector.exe  s3://$S3_BUCKET/windows/amd64/meshconnector.exe
zip -rj meshconnector_windows_amd64.zip meshconnector.exe 
aws s3  --profile $PROFILE --region us-west-2 cp meshconnector_windows_amd64.zip  s3://$S3_BUCKET/windows/meshconnector_windows_amd64.zip
