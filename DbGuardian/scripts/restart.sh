#!/bin/sh

REGION=us-west-2
# ARN="411064808315"
# REPO="db-sync"

PROFILE="dymium"

if [ "$1" = "staging" ]
then
    PROFILE="dymium_staging"
    shift
elif [ "$1" = "dev" ]
then
    PROFILE="dymium_dev"
    shift
fi

case "$1" in
  "")
      m="d-spoofcorp"
      ;;
  *)
      m="d-$1"
      ;;
esac
   
aws ecs update-service --cluster data-guardian-cluster --service ${m}-srv --force-new-deployment --profile ${PROFILE} --region ${REGION}
