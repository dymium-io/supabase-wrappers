#!/bin/bash

set -e
set -x

tag="latest"
if [ -n "$2" ]
then
  tag="$2"
fi


# Default to 'dev' if no argument is provided
ENVIRONMENT=${1:-dev}
PROFILE=dymium
# Determine the S3 bucket based on the environment
case $ENVIRONMENT in
  dev)
    ID="t0k4e6u4"
    PROFILE="dymium-dev"
    ;;
  prod)
    ID="b9j4u7x9"
    PROFILE="dymium-prod"
    ;;
  stage)
    ID="y9p2n4j2"
    PROFILE="dymium-stage"
    ;;
  *)
    echo "Invalid environment. Please specify 'dev', 'prod', or 'stage'."
    exit 1
    ;;
esac

docker tag machinetunnel:latest public.ecr.aws/${ID}/dymiummachinetunnel:${tag}
docker tag machinetunnel:latest public.ecr.aws/${ID}/dymiummachinetunnel:latest 
aws ecr-public get-login-password --region us-east-1 --profile ${PROFILE} | docker login --username AWS --password-stdin public.ecr.aws/${ID}

docker push public.ecr.aws/${ID}/dymiummachinetunnel:latest
docker tag  public.ecr.aws/${ID}/dymiummachinetunnel:latest public.ecr.aws/${ID}/dymiummachinetunnel:${tag}
