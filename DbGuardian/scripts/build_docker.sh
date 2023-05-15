#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

instantclient_version="$(${setup_d}/oracle.sh version linux)"
instantclients_all=($(${setup_d}/oracle.sh packages linux))

declare -a instantclients
for c in ${instantclients_all[@]}; do
	[[ $c == *basic* ]] && {
		instantclients+=($c)
	}
done

for f in ${instantclients[@]}; do
	[ -f ${setup_d}/oracle/$f ] || {
		echo "Oracle instantclient file $f not found in ${setup_d}"
		echo "Please use ${setup_d}/oracle.sh script to get them"
		exit -1
	}
done

[ -d $build_d ] && {
	rm -rf $build_d
}

mkdir $build_d

(
	cd ${script_d}/../go
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		go build -a -ldflags '-w -extldflags "-static"' -o "${build_d}/initializer"
)

retval=$?
[ $retval -ne 0 ] && {
	echo "build failed with error code $retval"
	exit $retval
}

fdws=(postgres_fdw mysql_fdw tds_fdw oracle_fdw)
(
	cd $script_d/../foreign_data_wrappers
	for f in ${fdws[@]}; do
		docker run -it --rm -v $PWD:/fdw postgres-dev /bin/sh -c \
			"cd /fdw/$f; make USE_PGXS=true; DESTDIR=/fdw make USE_PGXS=true install"
	done
)

cd $build_d
mv ../../foreign_data_wrappers/usr .
COPYFILE_DISABLE=1 tar czv --uid 0 --gid 0 -f usr.tar.gz usr
rm -rf usr

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

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
FROM ubuntu/postgres

RUN apt update &&                \
  apt upgrade -y &&              \
  apt install -y ca-certificates \
    libmariadb3 libmariadb-dev   \
    libmysqlclient21             \
    freetds-bin                  \
    libaio1 &&                   \
  ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so.21 /usr/lib/x86_64-linux-gnu/libmysqlclient.so && \
  mkdir -p /opt/oracle

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh

COPY usr.tar.gz /
RUN tar xzvf /usr.tar.gz && rm /usr.tar.gz

COPY ${instantclient_version}/ /opt/oracle/${instantclient_version}/
RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

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

ENTRYPOINT ["/usr/local/bin/startup.sh"]

EOF
