#!/bin/zsh

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

[ -d $build_d ] && {
	rm -rf $build_d
}

mkdir $build_d

(
	cd ${script_d}/../go
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		go build -a -ldflags '-w -extldflags "-static"' -o "${build_d}/logsupervisor"
)

retval=$?
[ $retval -ne 0 ] && {
	echo "build failed with error code $retval"
	exit $retval
}


cat <<EOF | docker build  -t postgres-splogging -f - .
#FROM ubuntu/postgres
FROM postgres:14.7
#####
RUN addgroup nobody tty
RUN addgroup postgres tty


# forward postgres logs to docker log collector
RUN mkdir -p /var/log/postgres
RUN chown postgres:postgres /var/log/postgres
USER postgres

RUN ln -sf /dev/stdout /var/log/postgres/postgres.csv \
	&& ln -sf /dev/stderr /var/log/postgres/postgres.log

COPY --chown=postgres ./docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod a+x /usr/local/bin/docker-entrypoint.sh

COPY --chown=postgres ./BLD/logsupervisor /usr/local/bin/logsupervisor
RUN chmod a+x /usr/local/bin/logsupervisor

# FOR LOCAL TESTS ONLY
ENV LOCAL_ENVIRONMENT=true
#####
ENTRY....
EOF
