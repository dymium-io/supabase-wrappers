#!/usr/bin/env bash
echo "Starting Log Collector $@"
/usr/local/bin/logcollector /tmp/logpipe &

/usr/local/bin/docker-entrypoint.sh postgres "$@" -c log_destination='csvlog' \
                                                          -c log_statement=all \
                                                          -c logging_collector=on \
                                                  	-c log_filename='postgres.log'\
                                                          -c log_directory='/var/log/postgres/'
