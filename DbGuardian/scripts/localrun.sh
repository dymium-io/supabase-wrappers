#!/bin/bash

PORT=9090

LOG_MIN_MESSAGES=${LOG_MIN_MESSAGES-WARNING}
LOG_MIN_ERROR_STATEMENT=${LOG_MIN_ERROR_STATEMENT-ERROR}

DATABASE_HOST=${DATABASE_HOST:-data-guardian.local}
DATABASE_PORT=${DATABASE_PORT:-9090}
DATABASE_USER=${DATABASE_USER:-dymium}
CUSTOMER=${CUSTOMER:-spoofcorp}
#LOCAL_SEARCH=${LOCAL_SEARCH-http://elasticsearch.dymium.local:9200}
#LOCAL_SEARCH_USER=${LOCAL_SEARCH_USER:-elastic}
#LOCAL_SEARCH_PASSWD=${LOCAL_SEARCH_PASSWD:-admin123}
#SEARCH_IN_PIPELINE=${SEARCH_IN_PIPELINE:-jsonmessage}

LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"

[ -z "$POSTGRES_PASSWORD" ] && {
	POSTGRES_PASSWORD=$(grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:postgres:" $HOME/.pgpass | cut -f 5 -d :)
}

[ -z "$DATABASE_PASSWORD" ] && {
	DATABASE_PASSWORD=$(grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d :)
}

set -x
docker run --rm --name ${CUSTOMER}.guardian.local --add-host=host.docker.internal:host-gateway \
	--network dymium \
	-p ${PORT}:5432 \
	-e LANG="en" \
	-e LC_ALL="$LC_ALL" \
	-e LC_CTYPE="$LC_CTYPE" \
	-e PLAIN_OUTPUT="$PLAIN_OUTPUT" \
	-e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
	-e DATABASE_USER=$DATABASE_USER \
	-e DATABASE_PASSWORD="$DATABASE_PASSWORD" \
	-e CUSTOMER=$CUSTOMER \
	-e LOCAL_ENVIRONMENT=true \
	-e LOCAL_SEARCH=$LOCAL_SEARCH \
	-e LOCAL_SEARCH_USER=$LOCAL_SEARCH_USER \
	-e LOCAL_SEARCH_PASSWD=$LOCAL_SEARCH_PASSWD \
	-e SEARCH_IN_PIPELINE=$SEARCH_IN_PIPELINE \
	-e AWS_LAMBDAS='{ "DbSync": "db-sync.dymium.local:8080" }' \
	-e log_min_messages=$LOG_MIN_MESSAGES \
	-e log_min_error_statement=$LOG_MIN_ERROR_STATEMENT \
	data-guardian
