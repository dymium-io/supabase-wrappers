#!/bin/sh

FUNCTION_NAME=DbAnalyzer
REGION=us-west-2
REPO="db-analyzer"

ARN="411064808315"
PROFILE="dymium"

if [ "$1" = "staging" ]
then
    ARN="626593984035"
    PROFILE="dymium_staging"
    shift
elif [ "$1" = "dev" ]
then
    ARN="564835066653"
    PROFILE="dymium_dev"
    shift
fi

set -x
aws lambda update-function-code \
    --function-name ${FUNCTION_NAME} \
    --image-uri ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:latest \
    --publish \
    --profile ${PROFILE} \
    --region ${REGION}
