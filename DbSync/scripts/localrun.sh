#!/bin/bash

PORT=9081

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}
DATABASE_TLS=${DATABASE_TLS:-disable}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}


# localhost name as visible from within the docker:
# docker.for.mac.host.internal

set -x
docker run --rm  --name db-sync           \
       -p ${PORT}:8080                         \
       -e DATABASE_HOST=docker.for.mac.host.internal \
       -e DATABASE_PORT=$DATABASE_PORT               \
       -e DATABASE_DB=$DATABASE_DB                   \
       -e DATABASE_USER=$DATABASE_USER               \
       -e DATABASE_PASSWORD=$DATABASE_PASSWORD       \
       -e DATABASE_TLS=$DATABASE_TLS                 \
       db-sync                                       \
       /main
