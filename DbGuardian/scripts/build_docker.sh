#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup


# build log collector
lc_build_d=${script_d}/../../logcollector/scripts/BLD

[ -d $lc_build_d ] && {
	rm -rf $lc_build_d
}

mkdir $lc_build_d

(
	cd ${lc_build_d}/../../go
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		go build -a -ldflags '-w -extldflags "-static"' -o "${lc_build_d}/logcollector"
)

retval=$?
[ $retval -ne 0 ] && {
	echo "build failed with error code $retval"
	exit $retval
}
cp ${lc_build_d}/logcollector ${build_d}/logcollector
cp $build_d/../startup.sh $build_d/startup.sh
cd $build_d

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

cat <<EOF | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t data-guardian -f - .
FROM guardian-base

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh


# Add logcollector
RUN addgroup nobody tty && addgroup postgres tty

# forward postgres logs to docker log collector
RUN mkdir -p /var/log/postgres && chown postgres:postgres /var/log/postgres && chmod a+rwx /var/log/postgres && \
mkdir -p /tmp && mkfifo /tmp/logpipe && chown postgres:postgres /tmp/logpipe && chmod 0644 /tmp/logpipe && \
mkfifo /tmp/errpipe && chown postgres:postgres /tmp/errpipe && chmod 0644 /tmp/errpipe

RUN ln -sf /tmp/logpipe /var/log/postgres/postgres.csv && ln -sf /tmp/errpipe /var/log/postgres/postgres.log

COPY ./logcollector /usr/local/bin/logcollector
RUN chmod a+x /usr/local/bin/logcollector

COPY ./startup.sh /usr/local/bin/startup.sh
RUN chmod a+x /usr/local/bin/startup.sh

ENV LD_LIBRARY_PATH="/usr/local/libmongo:/lib64:/usr/lib64:/lib/x86_64-linux-gnu:/var/lib/postgresql/sqllib/lib64:/var/lib/postgresql/sqllib/lib64/gskit:/var/lib/postgresql/sqllib/lib32"

RUN echo "en_US.UTF-8 UTF-8"> /etc/locale.gen
RUN locale-gen

ENTRYPOINT ["/usr/local/bin/startup.sh"]

EOF
