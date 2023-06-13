#!/bin/bash

set -e

REPO="db-analyzer"

source "../../libs/shell/aws-include.sh"
aws_params "$@"

docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}

set -x
aws ecr get-login-password --profile $PROFILE --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker pull ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}
docker tag ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag} ${REPO}:latest
