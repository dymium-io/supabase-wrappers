#!/bin/sh

DB_CLUSTER=${DB:-localhost}
DB_PORT=${DB_PORT:-5432}
DATABASE=${DATABASE:-dymium}
USER=${USER:-dymium}

../bin/mallard migrate -s public -r public \
	       --host ${DB_CLUSTER} --port ${DB_PORT} \
	       --user ${USER} --database ${DATABASE} \
	       --apply

../bin/mallard migrate -s global -r global \
	       --host ${DB_CLUSTER} --port ${DB_PORT} \
	       --user ${USER} --database ${DATABASE} \
	       --apply

for s in spoofcorp
do
    ../bin/mallard migrate -s $s -r customer \
	       --host ${DB_CLUSTER} --port ${DB_PORT} \
	       --user ${USER} --database ${DATABASE} \
	       --apply
done
