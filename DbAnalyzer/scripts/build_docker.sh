#!/bin/bash

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup
ktDbAnalyzerSvc_d=${script_d}/../../ktDbAnalyzerSvc

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

instantclient_version="$(${setup_d}/oracle.sh version linux)"
instantclients=($(${setup_d}/oracle.sh packages linux))

for f in ${instantclients[@]}; do
	[ -f ${setup_d}/oracle/$f ] || {
		echo "Oracle instantclient file $f not found in ${setup_d}"
		echo "Please use ${setup_d}/oracle.sh script to get them"
		exit -1
	}
done

cd $build_d

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

#DB2 libs
unzip ${setup_d}/db2/db2_home.zip
cp ${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz ${build_d}

echo Building DbAnalyzer...
/usr/bin/tar czf DbAnalyzer.tgz -C ../../.. DbAnalyzer/go
/usr/bin/tar czf libs.tgz -C ../../.. libs/go
docker run -it --rm \
                   -v ./:/src      \
                   -v $script_d/.go/:/go/ \
                   db-dev          \
                   /bin/sh -c      \
		   "cd /src && \
                    tar --warning=no-unknown-keyword -xf DbAnalyzer.tgz && \
                    tar --warning=no-unknown-keyword -xf libs.tgz && \
                    cd DbAnalyzer/go &&\
                    GOOS=linux GOARCH=amd64 \
                    go build -buildvcs=false -a -ldflags '-w ' -o /src/main"

retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}

cd $build_d

cp ../entry.sh .
cp ${setup_d}/ktDbAnalyzerSvc-0.0.1.tar .


# creating docker
DbAnalyzer=$(docker images db-analyzer -q)
[ -z "$DbAnalyzer" ] || docker rmi -f "$DbAnalyzer"


cat <<EOF | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t db-analyzer -f - .
FROM public.ecr.aws/lambda/provided:al2

COPY main entry.sh /

RUN mkdir -p /opt/oracle
COPY ./instantclient* /opt/oracle/

RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    /sbin/ldconfig

ENV ORACLE_HOME=/opt/oracle/${instantclient_version}

RUN yum install -y tar gzip pam java-11-amazon-corretto-headless && \
    yum clean all
RUN yum update -y curl 
RUN yum update -y nghttp2
RUN yum update -y python
RUN yum update -y
RUN mkdir /db2_cli_odbc_driver
COPY ./v11.5.4_linuxx64_odbc_cli.tar.gz /db2_cli_odbc_driver
RUN cd /db2_cli_odbc_driver && tar xvf v11.5.4_linuxx64_odbc_cli.tar.gz

ENV LD_LIBRARY_PATH="/db2_cli_odbc_driver/odbc_cli/clidriver/lib:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH"
ENV LIBPATH="/db2_cli_odbc_driver/odbc_cli/clidriver/lib:$LIBPATH"

RUN mkdir /ktDbAnalyzerSvc
COPY ./ktDbAnalyzerSvc-0.0.1.tar /ktDbAnalyzerSvc
RUN tar -xvf /ktDbAnalyzerSvc/ktDbAnalyzerSvc-0.0.1.tar -C /ktDbAnalyzerSvc && \
    rm -rf /ktDbAnalyzerSvc/ktDbAnalyzerSvc-0.0.1.tar

ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN chmod 755 /usr/bin/aws-lambda-rie /entry.sh /main
ENTRYPOINT ["/entry.sh"]

EOF
