#!/bin/bash
LOCAL_SEARCH=${LOCAL_SEARCH:-http://elastic-es-1:9200}
LOCAL_SEARCH_USER=${LOCAL_SEARCH_USER:-elastic}
LOCAL_SEARCH_PASSWD=${LOCAL_SEARCH_PASSWD:-admin123}
SEARCH_IN_PIPELINE=${SEARCH_IN_PIPELINE:-}

PORT=9080

echo "Starting on port $PORT"
docker run --rm  --name db-analyzer.dymium.local \
       --network dymium \
       -p ${PORT}:8080 \
              -e LOCAL_ENVIRONMENT=true                     \
              -e LOCAL_SEARCH=$LOCAL_SEARCH                 \
              -e LOCAL_SEARCH_USER=$LOCAL_SEARCH_USER \
              -e LOCAL_SEARCH_PASSWD=$LOCAL_SEARCH_PASSWD         \
              -e SEARCH_IN_PIPELINE=$SEARCH_IN_PIPELINE         \
       db-analyzer \
       /main
