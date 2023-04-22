#!/bin/zsh

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

[ -d $build_d ] && {
	rm -rf $build_d
}

mkdir $build_d

(
	cd ${script_d}/../../go
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		go build -a -ldflags '-w -extldflags "-static"' -o "${build_d}/logcollector"
)

retval=$?
[ $retval -ne 0 ] && {
	echo "build failed with error code $retval"
	exit $retval
}


cat <<EOF | docker build  -t postgres-splogging -f - .
FROM ubuntu/postgres
#####
RUN addgroup nobody tty
RUN addgroup postgres tty


# forward postgres logs to docker log collector
RUN mkdir -p /var/log/postgres
RUN chown postgres:postgres /var/log/postgres
RUN mkdir -p /tmp
RUN mkfifo /tmp/logpipe
RUN chmod a+rw /tmp/logpipe

RUN ln -sf /tmp/logpipe /var/log/postgres/postgres.csv
RUN ln -sf /dev/stderr /var/log/postgres/postgres.log

#COPY --chown=postgres ./docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
#RUN chmod a+x /usr/local/bin/docker-entrypoint.sh

COPY --chown=postgres ./BLD/logcollector /usr/local/bin/logcollector
RUN chmod a+x /usr/local/bin/logcollector

COPY --chown=postgres ./startup.sh /usr/local/bin/startup.sh
RUN chmod a+x /usr/local/bin/startup.sh

# FOR LOCAL TESTS ONLY
ENV LOCAL_ENVIRONMENT=true
ENV CUSTOMER=spoofcorp
#####

ENTRYPOINT ["/usr/local/bin/startup.sh"]
EOF
