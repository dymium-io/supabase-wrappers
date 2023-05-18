#!/bin/bash

set -e

FUNCTION_NAME=DbSync
REPO="db-sync"

source "../../libs/shell/aws-include.sh"
aws_params "$@"

set -x
aws lambda update-function-code \
    --function-name ${FUNCTION_NAME} \
    --image-uri ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:latest \
    --publish \
    --profile ${PROFILE} \
    --region ${REGION}
