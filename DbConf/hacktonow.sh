#!/usr/bin/env bash

set -e

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}

echo ../bin/mallard hack repair-checksum -r customer $1\
		--host ${DATABASE_HOST} --port ${DATABASE_PORT} \
		--user ${DATABASE_USER} --database ${DATABASE_DB} 


../bin/mallard hack repair-checksum -r customer $1\
		--host ${DATABASE_HOST} --port ${DATABASE_PORT} \
		--user ${DATABASE_USER} --database ${DATABASE_DB} 
