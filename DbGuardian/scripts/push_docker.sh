#!/bin/bash

set -e

REPO="data-guardian"
source "../../libs/shell/aws-include.sh"
aws_params "$@"

echo docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}

set -x
aws ecr get-login-password \
    --profile $PROFILE \
    --region $REGION |
    docker login --username AWS \
	   --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
echo docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}
