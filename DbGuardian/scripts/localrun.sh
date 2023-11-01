#!/bin/bash


LOG_MIN_MESSAGES=${LOG_MIN_MESSAGES-WARNING}
LOG_MIN_ERROR_STATEMENT=${LOG_MIN_ERROR_STATEMENT-ERROR}

CUSTOMER=${CUSTOMER:-spoofcorp}
GUARDIAN_HOST=${GUARDIAN_HOST:-${CUSTOMER}.guardian.local}
GUARDIAN_PORT=${GUARDIAN_PORT:-9090}

# this is used only to extract default passwords,
# assuming they are the same as for the dymium database
DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}

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

[ -z "$GUARDIAN_ADMIN_PASSWORD" ] && {
	GUARDIAN_ADMIN_PASSWORD=$(grep "^$GUARDIAN_HOST:\\(5432\\|[*]\\):[^:]*:postgres:" $HOME/.pgpass | cut -f 5 -d :)
	GUARDIAN_ADMIN_PASSWORD=${GUARDIAN_ADMIN_PASSWORD:-$POSTGRES_PASSWORD}
}

[ -z "$GUARDIAN_PASSWORD" ] && {
	GUARDIAN_PASSWORD=$(grep "^$GUARDIAN_HOST:\\(5432\\|[*]\\):[^:]*:dymium:" $HOME/.pgpass | cut -f 5 -d :)
	GUARDIAN_PASSWORD=${GUARDIAN_PASSWORD:-$DATABASE_PASSWORD}
}


[ -z "$GUARDIAN_PASSWORD" -o -z "$GUARDIAN_ADMIN_PASSWORD" ] && {
    echo "DbGuardian passwords are not defined"
    echo "Please add the following records to ~/.pgpass:"
    echo "${GUARDIAN_HOST}:5432:*:postgres:<password1>"
    echo "${GUARDIAN_HOST}:5432:*:dymium:<password2>"
    echo
    echo "or, if you prefer, just define default values:"
    echo "${DATABASE_HOST}:${DATABASE_PORT}:*:postgres:<password1>"
    echo "${DATABASE_HOST}:${DATABASE_PORT}:*:dymium:<password2>"
    exit 255
}

set -x
docker run --rm --name ${GUARDIAN_HOST} --add-host=host.docker.internal:host-gateway \
	--network dymium \
	-p ${GUARDIAN_PORT}:5432 \
	-e LANG="en" \
	-e LC_ALL="$LC_ALL" \
	-e LC_CTYPE="$LC_CTYPE" \
	-e PLAIN_OUTPUT="${PLAIN_OUTPUT}" \
	-e POSTGRES_PASSWORD="${GUARDIAN_ADMIN_PASSWORD}" \
	-e DATABASE_USER=dymium \
	-e DATABASE_PASSWORD="${GUARDIAN_PASSWORD}" \
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
