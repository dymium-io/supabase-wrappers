#!/bin/bash

set -e

ARN="411064808315"
PROFILE="dymium"


docker tag dymiumconnector:latest public.ecr.aws/a9d3u0m7/dymiumconnector:latest
aws ecr-public get-login-password --region us-east-1 --profile dymium | docker login --username AWS --password-stdin public.ecr.aws/a9d3u0m7
docker push public.ecr.aws/a9d3u0m7/dymiumconnector:latest

