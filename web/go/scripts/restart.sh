#!/usr/bin/env bash

set -e

CLUSTER="dymium"
SERVICE="webserver-srv"

source "../../../libs/shell/aws-include.sh"
aws_params "$@"
   
set -x
aws ecs update-service \
    --cluster ${CLUSTER} \
    --service ${SERVICE} \
    --force-new-deployment \
    --profile ${PROFILE} \
    --region ${REGION}
