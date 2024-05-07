#!/usr/bin/env bash

set -e

REPO="aa8y/postgres-dataset"
REMOTEREPO="demopostgres"
REGION="us-west-2"

ARN="391714386929"
PROFILE="demo-spoofcorp"


tag="latest"


docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
aws ecr get-login-password --profile $PROFILE --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
