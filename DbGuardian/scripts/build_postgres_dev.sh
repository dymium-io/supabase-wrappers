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
cp ${setup_d}/db2/v11.5.8_linuxx64_rtcl.tar.gz ${build_d}
unzip ${setup_d}/db2/db2_home.zip

PostgresDev=$(docker images postgres-dev -q)
[ -z "$PostgresDev" ] || docker rmi -f "$PostgresDev"

cat <<EOF | docker build --platform linux/amd64 --compress -t postgres-dev -f - .
FROM ubuntu:jammy


RUN ln -fs /usr/share/zoneinfo/America/Los_Angeles /etc/localtime
RUN apt-get update &&            \
    apt-get upgrade -y &&        \
    apt-get install -y           \
      build-essential            \
      postgresql-14              \
      postgresql-server-dev-all  \
      unixodbc unixodbc-dev      \
      odbc-postgresql            \
      libmariadb3 libmariadb-dev \
      freetds-bin                \
      freetds-dev                \
      libaio1                    \
      curl wget cmake            \
      libreadline-dev            \
      flex                       \
      bison                      \
      openjdk-8-jdk              \
      pkg-config &&              \
    mkdir -p /opt/oracle

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY ./ /opt/oracle/

RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

ENV ORACLE_HOME=/opt/oracle/${instantclient_version}

RUN mkdir -p /var/lib/postgresql/db2rtcl
COPY v11.5.8_linuxx64_rtcl.tar.gz /var/lib/postgresql/db2rtcl
RUN (cd /var/lib/postgresql/db2rtcl; tar xvf v11.5.8_linuxx64_rtcl.tar.gz)

USER postgres
WORKDIR /var/lib/postgresql
RUN (cd /var/lib/postgresql/db2rtcl/rtcl;   ./db2_install -y -f sysreq)

USER root
WORKDIR /
RUN rm -rf /var/lib/postgresql/db2rtcl/rtcl

ENV CLASSPATH="/var/lib/postgresql/sqllib/java/db2java.zip:/var/lib/postgresql/sqllib/function:/var/lib/postgresql/sqllib/java/db2jcc_license_cu.jar:/var/lib/postgresql/sqllib/tools/clpplus.jar:/var/lib/postgresql/sqllib/tools/jline-0.9.93.jar:/var/lib/postgresql/sqllib/java/db2jcc4.jar:/var/lib/postgresql/sqllib/java/db2jcc_license_cisuz.jar:."
ENV DB2DIR="/var/lib/postgresql/sqllib"
ENV DB2INSTANCE="postgres"
ENV DB2LIB="/var/lib/postgresql/sqllib/lib"
#ENV DB2_HOME="/var/lib/postgresql/sqllib"
ENV DB2_NET_CLIENT_PATH=""
ENV IBM_DB_DIR="/var/lib/postgresql/sqllib"
ENV IBM_DB_HOME="/var/lib/postgresql/sqllib"
ENV IBM_DB_INCLUDE="/var/lib/postgresql/sqllib/include"
ENV IBM_DB_LIB="/var/lib/postgresql/sqllib/lib"
ENV INSTHOME="/var/lib/postgresql"
ENV INST_DIR="/var/lib/postgresql/sqllib"
ENV LD_LIBRARY_PATH="/lib64:/lib/x86_64-linux-gnu:/var/lib/postgresql/sqllib/lib64:/var/lib/postgresql/sqllib/lib64/gskit:/var/lib/postgresql/sqllib/lib32"
#ENV PATH="/usr/local/bin:/usr/bin:/bin:/var/lib/postgresql/sqllib/bin:/var/lib/postgresql/sqllib/adm:/var/lib/postgresql/sqllib/misc:/var/lib/postgresql/sqllib/pd:/var/lib/postgresql/sqllib/gskit/bin"

ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
ENV PATH="$PATH:/usr/lib/jvm/java-8-openjdk-amd64/bin"

COPY ./db2_home /opt/ibm/db2/V11.5
ENV DB2_HOME=/opt/ibm/db2/V11.5

RUN ln -s /usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64/server/libjvm.so /usr/lib64/libjvm.so

ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo install cargo-pgx 2>&1 | sed -u '/jemalloc/d'
RUN cargo pgx init --pg14 /usr/bin/pg_config 2>&1 | sed -u '/jemalloc/d'

EOF
