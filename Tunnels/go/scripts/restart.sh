#!/bin/sh

source "../../../libs/shell/aws-include.sh"

if [ "$1" = "-c" ]
then
    shift
    [ -z "$1" ] && {
	aws_usage -exe "$0 -c <customer> "
    }
    c="$1"
    shift
else
    aws_usage -exe "$0 -c <customer> "
fi

aws_params -exe "$0 -c <customer> " "$@"
   
set -x
aws ecs update-service \
    --cluster "tunnel-${c}" \
    --service "service-${c}" \
    --force-new-deployment \
    --profile ${PROFILE} \
    --region ${REGION}
