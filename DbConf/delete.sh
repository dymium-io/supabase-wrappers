#!/usr/bin/env bash

set -e

DATABASE_HOST=${DATABASE_HOST:-localhost}
DATABASE_PORT=${DATABASE_PORT:-5432}
DATABASE_DB=${DATABASE_DB:-dymium}
DATABASE_USER=${DATABASE_USER:-dymium}

[ -z "$1" ] && {
    echo "Usage: $0 <customer>"
    exit 255
}


../bin/mallard drop -s $1  \
		--host ${DATABASE_HOST} --port ${DATABASE_PORT} \
		--user ${DATABASE_USER} --database ${DATABASE_DB} 
	
