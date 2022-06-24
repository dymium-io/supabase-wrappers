#!/bin/sh

PROFILE=dymium
REGION=us-west-2
# ARN="411064808315"
# REPO="db-sync"


case "$1" in
  "")
      m="d-spoofcorp"
      ;;
  *)
      m="d-$1"
      ;;
esac
   
aws ecs update-service --cluster data-guardian-cluster --service ${m}-srv --force-new-deployment --profile ${PROFILE} --region ${REGION}
