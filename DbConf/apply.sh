#!/usr/bin/env bash

set -e

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}
CUSTOMER=${CUSTOMER:-spoofcorp}

../bin/mallard migrate -s public -r public \
	       --host ${DATABASE_HOST} --port ${DATABASE_PORT} \
	       --user ${DATABASE_USER} --database ${DATABASE_DB} \
	       --apply

../bin/mallard migrate -s global -r global \
	       --host ${DATABASE_HOST} --port ${DATABASE_PORT} \
	       --user ${DATABASE_USER} --database ${DATABASE_DB} \
               --apply

../bin/mallard migrate -s $CUSTOMER -r customer \
	       --host ${DATABASE_HOST} --port ${DATABASE_PORT} \
	       --user ${DATABASE_USER} --database ${DATABASE_DB} \
	       --apply
