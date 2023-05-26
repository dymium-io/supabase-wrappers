#!/usr/bin/env bash

[ -n "$PLAIN_OUTPUT" ] && {
	exec /usr/local/bin/docker-entrypoint.sh postgres "$@" \
		-c log_destination='stderr' \
		-c log_statement=all \
		-c log_min_messages=$log_min_messages \
		-c log_min_error_statement=$log_min_error_statement
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
