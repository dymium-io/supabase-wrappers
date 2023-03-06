#!/bin/bash

PORT=9081

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}
DATABASE_PAASSWORD=${DATABASE_PASSWORD:-$kdvnMsp4o}
DATABASE_TLS=${DATABASE_TLS:-false}
SPOOFCORP_KEY=${SPOOFCORP_KEY:-6874AB957AA1F505EC6ACC84162B131FA5513558BB64ACEF294388AE6ECDA9C9}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}

# localhost name as visible from within the docker:
# docker.for.mac.host.internal

set -x
docker run --rm  --name db-sync.dymium.local   \
       --network dymium                        \
       -p ${PORT}:8080                         \
       -e DATABASE_HOST=localhost              \
       -e DATABASE_PORT=$DATABASE_PORT         \
       -e DATABASE_DB=$DATABASE_DB             \
       -e DATABASE_USER=$DATABASE_USER         \
       -e DATABASE_TLS=$DATABASE_TLS           \
       -e DATABASE_PASSWORD=DATABASE_PASSWORD  \
       -e AWS_LAMBDAS="{}"                     \
       -e AWS_SECRETS="{
               \"DATABASE_PASSWORD\": \"$DATABASE_PASSWORD\",
               \"SPOOFCORP_PASSWORD\": \"$DATABASE_PASSWORD\",
               \"SPOOFCORP_KEY\": \"$SPOOFCORP_KEY\"
             }"                                \
       -e GUARDIAN_CONF="{
             \"DEFAULT\": {
                 \"guardian_address\": [\"localhost\"],
                 \"guardian_port\": 9090,
                 \"guardian_tls\": false,
                 \"guardian_user\": \"$DATABASE_USER\",
                 \"guardian_database\": \"postgres\"
             },
             \"spoofcorp\": {
                 \"guardian_password\": \"SPOOFCORP_PASSWORD\",
                 \"customer_aes_key\":  \"SPOOFCORP_KEY\"
             }
	  }"                                   \
       db-sync                                 \
       /main
