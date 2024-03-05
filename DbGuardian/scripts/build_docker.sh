#!/bin/zsh

set -e
script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

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

#get jdbc dependencies
jdbclibs=${setup_d}/ktDbAnalyzerSvc-0.0.1.tar
[ -f $jdbclibs ] || {
		echo "jdbc lib dependencies file not found in ${setup_d}"
		echo "Please build ktDbAnalyzerSvc to get them"
		exit -1
	}
cp $jdbclibs .

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

cat <<EOF | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t data-guardian -f - .
FROM guardian-base

RUN apt update &&                \
  apt upgrade -y 
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

COPY ./ktDbAnalyzerSvc-0.0.1.tar .
RUN tar xf ktDbAnalyzerSvc-0.0.1.tar && mkdir /jdbc_drv && cp ktDbAnalyzerSvc-0.0.1/lib/* /jdbc_drv && rm -rf ktDbAnalyzerSvc-0.0.1

#ENV CLASSPATH="$CLASSPATH:/jdbc_drv/lib/ktDbAnalyzerSvc-0.0.1.jar:/jdbc_drv/lib/kotlin-logging-jvm-5.0.1.jar:/jdbc_drv/lib/ktor-server-call-id-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-server-call-logging-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-server-content-negotiation-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-server-netty-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-server-host-common-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-server-core-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-serialization-kotlinx-json-jvm-2.3.5.jar:/jdbc_drv/lib/kotlin-result-jvm-1.1.18.jar:/jdbc_drv/lib/ktor-serialization-kotlinx-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-serialization-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-events-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-websockets-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-http-cio-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-http-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-network-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-utils-jvm-2.3.5.jar:/jdbc_drv/lib/ktor-io-jvm-2.3.5.jar:/jdbc_drv/lib/kotlinx-coroutines-jdk8-1.7.3.jar:/jdbc_drv/lib/kotlinx-coroutines-core-jvm-1.7.3.jar:/jdbc_drv/lib/kotlinx-coroutines-slf4j-1.7.3.jar:/jdbc_drv/lib/kotlin-stdlib-jdk8-1.9.10.jar:/jdbc_drv/lib/logback-classic-1.4.11.jar:/jdbc_drv/lib/gson-2.10.1.jar:/jdbc_drv/lib/mysql-connector-j-8.1.0.jar:/jdbc_drv/lib/postgresql-42.6.0.jar:/jdbc_drv/lib/ojdbc8-23.3.0.23.09.jar:/jdbc_drv/lib/jcc-11.5.8.0.jar:/jdbc_drv/lib/x-pack-sql-jdbc-8.10.4.jar:/jdbc_drv/lib/mssql-jdbc-12.2.0.jre11.jar:/jdbc_drv/lib/mariadb-java-client-3.1.4.jar:/jdbc_drv/lib/kotlin-stdlib-jdk7-1.9.10.jar:/jdbc_drv/lib/kotlin-reflect-1.8.22.jar:/jdbc_drv/lib/atomicfu-jvm-0.22.0.jar:/jdbc_drv/lib/kotlinx-serialization-core-jvm-1.5.1.jar:/jdbc_drv/lib/kotlinx-serialization-json-jvm-1.5.1.jar:/jdbc_drv/lib/kotlin-stdlib-1.9.10.jar:/jdbc_drv/lib/logback-core-1.4.11.jar:/jdbc_drv/lib/waffle-jna-3.2.0.jar:/jdbc_drv/lib/jcl-over-slf4j-1.7.36.jar:/jdbc_drv/lib/slf4j-api-2.0.7.jar:/jdbc_drv/lib/protobuf-java-3.21.9.jar:/jdbc_drv/lib/caffeine-2.9.3.jar:/jdbc_drv/lib/checker-qual-3.31.0.jar:/jdbc_drv/lib/kotlin-stdlib-common-1.9.10.jar:/jdbc_drv/lib/config-1.4.2.jar:/jdbc_drv/lib/jansi-2.4.0.jar:/jdbc_drv/lib/netty-codec-http2-4.1.97.Final.jar:/jdbc_drv/lib/alpn-api-1.1.3.v20160715.jar:/jdbc_drv/lib/netty-transport-native-kqueue-4.1.97.Final.jar:/jdbc_drv/lib/netty-transport-native-epoll-4.1.97.Final.jar:/jdbc_drv/lib/annotations-23.0.0.jar:/jdbc_drv/lib/jna-platform-5.12.1.jar:/jdbc_drv/lib/jna-5.12.1.jar:/jdbc_drv/lib/netty-codec-http-4.1.97.Final.jar:/jdbc_drv/lib/netty-handler-4.1.97.Final.jar:/jdbc_drv/lib/netty-codec-4.1.97.Final.jar:/jdbc_drv/lib/netty-transport-classes-kqueue-4.1.97.Final.jar:/jdbc_drv/lib/netty-transport-classes-epoll-4.1.97.Final.jar:/jdbc_drv/lib/netty-transport-native-unix-common-4.1.97.Final.jar:/jdbc_drv/lib/netty-transport-4.1.97.Final.jar:/jdbc_drv/lib/netty-buffer-4.1.97.Final.jar:/jdbc_drv/lib/netty-resolver-4.1.97.Final.jar:/jdbc_drv/lib/netty-common-4.1.97.Final.jar:/jdbc_drv/lib/error_prone_annotations-2.10.0.jar"


COPY ./startup.sh /usr/local/bin/startup.sh
RUN chmod a+x /usr/local/bin/startup.sh

RUN echo "en_US.UTF-8 UTF-8"> /etc/locale.gen
RUN locale-gen

ENTRYPOINT ["/usr/local/bin/startup.sh"]

EOF
