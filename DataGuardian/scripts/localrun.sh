#!/bin/bash

PORT=9090

DATABASE_HOST=${DATABASE_HOST:-data-guardian.local}
DATABASE_PORT=${DATABASE_PORT:-9090}
DATABASE_USER=${DATABASE_USER:-dymium}
CUSTOMER=${CUSTOMER:-spoofcorp}
TEST_USER=${TEST_USER:-schwinger}
TEST_PASSWORD=${TEST_PASSWORD:-WhoIsMrFeynman?}

[ -z "$POSTGRES_PASSWORD" ] && {
    POSTGRES_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:postgres:" $HOME/.pgpass | cut -f 5 -d : )
}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}

set -x
docker run --rm  --name data-guardian                \
       -p ${PORT}:5432                               \
       -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD"     \
       -e DATABASE_USER=$DATABASE_USER               \
       -e DATABASE_PASSWORD="$DATABASE_PASSWORD"     \
       -e TEST_USER=$TEST_USER                       \
       -e TEST_PASSWORD="$TEST_PASSWORD"             \
       -e CUSTOMER=$CUSTOMER                         \
       -e AWS_LAMBDAS='{ "DbSync": "docker.for.mac.host.internal:9081" }' \
       data-guardian
