#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

instantclient_version="$(${setup_d}/oracle.sh version linux)"
instantclients=($(${setup_d}/oracle.sh packages linux))

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
cd $build_d

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

#DB2 libs
unzip ${setup_d}/db2/db2_home.zip
#cp db2_home ${build_d}
cp ${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz ${build_d}

DbDev=$(docker images db-dev -q)
[ -z "$DbDev" ] || docker rmi -f "$DbDev"

cat <<EOF | docker build --platform linux/amd64 --compress -t db-dev -f - .
FROM golang:1.20-buster


RUN apt-get update &&            \
    apt-get upgrade -y &&        \
    apt-get install -y           \
      build-essential            \
      postgresql-server-dev-all  \
      libmariadb3 libmariadb-dev \
      freetds-dev                \
      libaio1 &&                 \
    mkdir -p /opt/oracle

COPY ./instantclient* /opt/oracle/

RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

ENV ORACLE_HOME=/opt/oracle/${instantclient_version}

ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie

ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN apt-get --allow-releaseinfo-change update
RUN apt-get update && apt-get install -y \
        freetds-bin \
        freetds-dev \
        freetds-common

RUN mkdir /db2_cli_odbc_driver
COPY ./v11.5.4_linuxx64_odbc_cli.tar.gz /db2_cli_odbc_driver
RUN cd /db2_cli_odbc_driver && tar xvf v11.5.4_linuxx64_odbc_cli.tar.gz

COPY ./db2_home/include /db2_cli_odbc_driver/odbc_cli/clidriver/include

ENV DB2_CLI_DRIVER_INSTALL_PATH="/db2_cli_odbc_driver/odbc_cli/clidriver"
ENV LD_LIBRARY_PATH="/db2_cli_odbc_driver/odbc_cli/clidriver/lib:/opt/ibm/db2/V11.5/lib64:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH"
ENV LIBPATH="/db2_cli_odbc_driver/odbc_cli/clidriver/lib:$LIBPATH"
ENV IBM_DB_HOME="/db2_cli_odbc_driver/odbc_cli/clidriver"
ENV CGO_CFLAGS="-I/db2_cli_odbc_driver/odbc_cli/clidriver/include"
ENV CGO_LDFLAGS="-L/db2_cli_odbc_driver/odbc_cli/clidriver/lib"


#ENV PATH="/db2_cli_odbc_driver/odbc_cli/clidriver/include:$PATH"


RUN apt-get install -y unixodbc unixodbc-dev

#ENV PATH="$PATH:/opt/ibm/db2/V11.5/inclide"
EOF
