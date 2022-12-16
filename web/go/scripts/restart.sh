#!/bin/sh

CLUSTER="webserver"
SERVICE="w"

source "../../../libs/shell/aws-include.sh"
aws_params "$@"
   
set -x
aws ecs update-service \
    --cluster ${CLUSTER} \
    --service ${SERVICE} \
    --force-new-deployment \
    --profile ${PROFILE} \
    --region ${REGION}
