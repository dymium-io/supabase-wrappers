#!/usr/bin/env bash
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
locale-gen en_US.UTF-8
dpkg-reconfigure locales

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
