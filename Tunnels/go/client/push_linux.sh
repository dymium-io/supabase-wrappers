#!/usr/bin/env bash

set -e

# make the script output its commands before executing them
set -x

# Default to 'dev' if no argument is provided
ENVIRONMENT=${1:-dev}
PROFILE=dymium
# Determine the S3 bucket based on the environment
case $ENVIRONMENT in
  dev)
    S3_BUCKET="dymium-dev-tunneling-clients"
    PROFILE="dymium-dev"
    ;;
  prod)
    S3_BUCKET="dymium-prod-tunneling-clients"
    PROFILE="dymium-prod"
    ;;
  stage)
    S3_BUCKET="dymium-stage-tunneling-clients"
    PROFILE="dymium-stage"
    ;;
  *)
    echo "Invalid environment. Please specify 'dev', 'prod', or 'stage'."
    exit 1
    ;;
esac

CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o dymium

tar cvzf tunnel.tar.gz -C ./ dymium
aws s3  --profile $PROFILE --region us-west-2 cp tunnel.tar.gz  s3://$S3_BUCKET/linux/tunnel.tar.gz

go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o dymium
