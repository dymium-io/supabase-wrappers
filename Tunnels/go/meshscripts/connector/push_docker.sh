#!/bin/bash

set -e

REPO="meshconnector"
REMOTEREPO="meshconnector"
REGION="us-west-2"

case "$1" in
    "dymium")
	ARN="411064808315"
	PROFILE="dymium"
	shift
	;;
    "demo-spoofcorp")
	ARN="391714386929"
	PROFILE="demo-spoofcorp"
	shift
	;;
#    "dev")
#	ARN="564835066653"
#	PROFILE="dymium_dev"
#	shift
#	;;
    *)
	echo "option '$1' is not supported"
	echo "Usage: $0 [dymium|staging|dev] [latest|prod]"
	exit 255
	;;
esac

tag="${1:-latest}"
[ $tag = "latest" -o $tag = "prod" ] || {
    echo "Usage: ./push_docker.sh [tag]"
    echo "  where tag must be equal to 'latest' or 'prod'"
    exit 1
}


echo "docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}"
echo "aws ecr get-login-password --profile $PROFILE --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com"
echo "docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}"


docker tag ${REPO}:latest ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
aws ecr get-login-password --profile $PROFILE --region $REGION | docker login --username AWS --password-stdin ${ARN}.dkr.ecr.${REGION}.amazonaws.com
docker push ${ARN}.dkr.ecr.${REGION}.amazonaws.com/${REMOTEREPO}:${tag}
