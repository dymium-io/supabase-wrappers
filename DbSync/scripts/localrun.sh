#!/bin/bash

set -e

PORT=${PORT:-9081}

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}
DATABASE_TLS=${DATABASE_TLS:-false}
LOCAL_SEARCH=${LOCAL_SEARCH:-}
LOCAL_SEARCH_USER=${LOCAL_SEARCH_USER:-elastic}
LOCAL_SEARCH_PASSWD=${LOCAL_SEARCH_PASSWD:-admin123}
SEARCH_IN_PIPELINE=${SEARCH_IN_PIPELINE:-}

SPOOFCORP_KEY=${SPOOFCORP_KEY:-6874AB957AA1F505EC6ACC84162B131FA5513558BB64ACEF294388AE6ECDA9C9}
SPOOFCORPPING_KEY=${SPOOFCORPPING_KEY:-"Bel5nOvtP1/OjDD6I6NC4p4d5Kz2/jYPs1C2cQCgP1s="}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}

[ -z "$DATABASE_PASSWORD" ] && {
    echo "Database password is not defined"
    echo "Please add the following records to ~/.pgpass:"
    echo "${DATABASE_HOST}:${DATABASE_PORT}:*:dymium:<password>"
    exit 255
}

SPOOFCORP_HOST=${SPOOFCORP_HOST:-spoofcorp.guardian.local}
SPOOFCORPPING_HOST=${SPOOFCORPPING_HOST:=spoofcorpping.guardian.local}

[ -z "$SPOOFCORP_PASSWORD" ] && {
	SPOOFCORP_PASSWORD=$(grep "^$SPOOFCORP_HOST:\\(5432\\|[*]\\):[^:]*:dymium:" $HOME/.pgpass | cut -f 5 -d :)
	SPOOFCORP_PASSWORD=${SPOOFCORP_PASSWORD:-$DATABASE_PASSWORD}
}

[ -z "$SPOOFCORPPING_PASSWORD" ] && {
	SPOOFCORPPING_PASSWORD=$(grep "^$SPOOFCORPPING_HOST:\\(5432\\|[*]\\):[^:]*:dymium:" $HOME/.pgpass | cut -f 5 -d :)
	SPOOFCORPPING_PASSWORD=${SPOOFCORPPING_PASSWORD:-$DATABASE_PASSWORD}
}

# localhost name as visible from within the docker:
[ "$(sed 's/#.*//' /etc/hosts | grep -w $DATABASE_HOST | cut -d ' ' -f 1)" = "127.0.0.1" ] && {
    DATABASE_HOST="host.docker.internal"
}

set -x
docker run --rm  --name db-sync.dymium.local   \
       --network dymium                        \
       -p ${PORT}:8080                         \
       -e DATABASE_HOST=$DATABASE_HOST         \
       -e DATABASE_PORT=$DATABASE_PORT         \
       -e DATABASE_DB=$DATABASE_DB             \
       -e DATABASE_USER=$DATABASE_USER         \
       -e DATABASE_TLS=$DATABASE_TLS           \
       -e DATABASE_PASSWORD=DATABASE_PASSWORD  \
       -e LOCAL_ENVIRONMENT=true               \
       -e LOCAL_SEARCH=$LOCAL_SEARCH           \
       -e LOCAL_SEARCH_USER=$LOCAL_SEARCH_USER \
       -e LOCAL_SEARCH_PASSWD=$LOCAL_SEARCH_PASSWD \
       -e SEARCH_IN_PIPELINE=$SEARCH_IN_PIPELINE   \
       -e AWS_LAMBDAS="{}"                     \
       -e AWS_SECRETS="{
               \"DATABASE_PASSWORD\": \"$DATABASE_PASSWORD\",
               \"SPOOFCORP_PASSWORD\": \"$SPOOFCORP_PASSWORD\",
               \"SPOOFCORPPING_PASSWORD\": \"$SPOOFCORPPING_PASSWORD\",
               \"SPOOFCORP_KEY\": \"$SPOOFCORP_KEY\",
               \"SPOOFCORPPING_KEY\": \"$SPOOFCORPPING_KEY\"
             }"                                \
       -e GUARDIAN_CONF="{
             \"DEFAULT\": {
                 \"guardian_port\": 5432,
                 \"guardian_tls\": false,
                 \"guardian_user\": \"$DATABASE_USER\",
                 \"guardian_database\": \"postgres\"
             },
             \"spoofcorp\": {
                 \"guardian_address\": [\"${SPOOFCORP_HOST}\"],
                 \"guardian_password\": \"SPOOFCORP_PASSWORD\",
                 \"customer_aes_key\":  \"SPOOFCORP_KEY\"
             },
             \"spoofcorpping\": {
                 \"guardian_address\": [\"${SPOOFCORPPING_HOST}\"],
                 \"guardian_password\": \"SPOOFCORPPING_PASSWORD\",
                 \"customer_aes_key\":  \"SPOOFCORPPING_KEY\"
             }

	  }"                                   \
       db-sync                                 \
       /main
