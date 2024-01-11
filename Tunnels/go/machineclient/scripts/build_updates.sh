#!/bin/bash

cd ../src

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
    S3_BUCKET="dymium-dev-machine-clients"
    PROFILE="dymium-dev"
    ;;
  prod)
    S3_BUCKET="dymium-prod-machine-clients"
    PROFILE="dymium-prod"
    ;;
  stage)
    S3_BUCKET="dymium-stage-machine-clients"
    PROFILE="dymium-stage"
    ;;
  *)
    echo "Invalid environment. Please specify 'dev', 'prod', or 'stage'."
    exit 1
    ;;
esac


CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o machineclient
chmod a+x machineclient     
aws s3  --profile $PROFILE --region us-west-2 cp machineclient  s3://$S3_BUCKET/linux/amd64/machineclient

CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o machineclient
chmod a+x machineclient     
aws s3  --profile $PROFILE --region us-west-2 cp machineclient  s3://$S3_BUCKET/darwin/amd64/machineclient


cd ../scripts
