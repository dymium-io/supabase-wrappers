#!/bin/bash

PORT=9090

DATABASE_HOST=${DATABASE_HOST:-data-guardian.local}
DATABASE_PORT=${DATABASE_PORT:-9090}
DATABASE_USER=${DATABASE_USER:-dymium}
CUSTOMER=${CUSTOMER:-spoofcorp}
LOCAL_SEARCH=${LOCAL_SEARCH:}

[ -z "$POSTGRES_PASSWORD" ] && {
    POSTGRES_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:postgres:" $HOME/.pgpass | cut -f 5 -d : )
}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}

set -x
docker run --rm  --name ${CUSTOMER}.guardian.local     \
       --network dymium                              \
       -p ${PORT}:5432                               \
       -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD"     \
       -e DATABASE_USER=$DATABASE_USER               \
       -e DATABASE_PASSWORD="$DATABASE_PASSWORD"     \
       -e CUSTOMER=$CUSTOMER                         \
       -e LOCAL_ENVIRONMENT=true                     \
       -e LOCAL_SEARCH=$LOCAL_SEARCH                 \
       -e AWS_LAMBDAS='{ "DbSync": "db-sync.dymium.local:8080" }' \
       data-guardian