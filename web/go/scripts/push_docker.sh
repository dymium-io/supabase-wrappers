#!/bin/bash

set -e

ARN="411064808315"
REPO="dymium"
REMOTEREPO="webserver"
REGION="us-west-2"

tag="${1:-latest}"
[ $tag = "latest" -o $tag = "prod" ] || {
    echo "Usage: ./push_docker.sh [tag]"
    echo "  where tag must be equal to 'latest' or 'prod'"
    exit 1
}

export AWS_PROFILE=dymium
docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}

aws ecr get-login-password --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
