#!/bin/bash

set -e

ARN="411064808315"
PROFILE="dymium"

tag="latest"
if [ -n "$1" ]
then
  tag="$1"
fi

docker tag dymiumconnector:latest public.ecr.aws/a9d3u0m7/dymiumcmachinetunnel:${tag}
docker tag dymiumconnector:latest public.ecr.aws/a9d3u0m7/dymiumcmachinetunnel:latest 
aws ecr-public get-login-password --region us-east-1 --profile dymium | docker login --username AWS --password-stdin public.ecr.aws/a9d3u0m7

docker push public.ecr.aws/a9d3u0m7/dymiumcmachinetunnel:latest
docker tag  public.ecr.aws/a9d3u0m7/dymiumcmachinetunnel:latest public.ecr.aws/a9d3u0m7/dymiumcmachinetunnel:${tag}
