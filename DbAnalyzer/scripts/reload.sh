#!/usr/bin/env bash

set -e

FUNCTION_NAME=DbAnalyzer
REPO="db-analyzer"

source "../../libs/shell/aws-include.sh"
aws_params "$@"

set -x
aws lambda update-function-code \
    --function-name ${FUNCTION_NAME} \
    --image-uri ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:$tag \
    --publish \
    --profile ${PROFILE} \
    --region ${REGION}
