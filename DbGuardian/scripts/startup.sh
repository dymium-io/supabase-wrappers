#!/usr/bin/env bash

[ -z "$NO_LOG_COLLECTOR" ]  || {
  exec /usr/local/bin/docker-entrypoint.sh postgres "$@"
}

echo "Starting Log Collector $@"
/usr/local/bin/logcollector -componentname data-guardian -pipename /tmp/logpipe -sourcename logstream &
/usr/local/bin/logcollector -componentname data-guardian -pipename /tmp/errpipe -sourcename errstream &

exec /usr/local/bin/docker-entrypoint.sh \
     postgres "$@" \
     -c log_destination='csvlog' \
     -c log_statement=all \
     -c logging_collector=on \
     -c log_filename='postgres.log' \
     -c log_directory='/var/log/postgres/'
