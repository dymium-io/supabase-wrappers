#!/bin/bash

set -e

REPO="dymium"
REMOTEREPO="webserver"

source "../../../libs/shell/aws-include.sh"
aws_params "$@"

docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}

set -x
aws ecr get-login-password \
    --profile $PROFILE \
    --region $REGION |
    docker login \
	   --username AWS \
	   --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
