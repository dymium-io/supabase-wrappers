#!/bin/sh

FUNCTION_NAME=DbSync
PROFILE=dymium
REGION=us-west-2
ARN="411064808315"
REPO="db-sync"

set -x
aws lambda update-function-code \
    --function-name ${FUNCTION_NAME} \
    --image-uri ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:latest \
    --publish \
    --profile ${PROFILE} \
    --region ${REGION}
