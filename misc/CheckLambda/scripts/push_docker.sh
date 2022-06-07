#!/bin/bash

set -e

ARN="411064808315"
REPO="check-lambda"
REGION="us-west-2"

tag="${1:-latest}"
[ $tag = "latest" -o $tag = "prod" ] || {
    echo "Usage: ./push_docker.sh [tag]"
    echo "  where tag must be equal to 'latest' or 'prod'"
    exit 1
}

docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}

aws ecr get-login-password --profile dymium --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REPO}:${tag}
