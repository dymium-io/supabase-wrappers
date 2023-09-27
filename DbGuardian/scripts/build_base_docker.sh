#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
setup_d=${script_d}/../../setup
wrk=${script_d}/.wrk

[ -d "$script_d" ] || {
    echo "Derectory $script_d does not exist"
    exit 255
}

[ -d "$wrk" ] && {
    rm -rf $wrk
}

mkdir $wrk
trap "rm -rf $wrk" EXIT
cd $wrk

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

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

#DB2 libs
cp ${setup_d}/db2/v11.5.8_linuxx64_rtcl.tar.gz .
#cp ${setup_d}/db2/odbcinst.ini .
cp ${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz .


# creating docker
GuardianBase=$(docker images guardian-base -q)
[ -z "$GuardianBase" ] || docker rmi -f "$GuardianBase"

cat <<EOF | docker build --platform linux/amd64 --compress -t guardian-base -f - .
FROM ubuntu/postgres

RUN apt update &&                \
  apt upgrade -y &&              \
  apt install -y ca-certificates \
    libmariadb3 libmariadb-dev   \
    libmysqlclient21             \
    freetds-bin                  \
    ksh unzip                    \
    openjdk-8-jdk              \
    libaio1 &&                   \
  ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so.21 /usr/lib/x86_64-linux-gnu/libmysqlclient.so && \
  mkdir -p /opt/oracle

COPY ${instantclient_version}/ /opt/oracle/${instantclient_version}/
RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

RUN mkdir -p /var/lib/postgresql/db2rtcl
COPY v11.5.8_linuxx64_rtcl.tar.gz /var/lib/postgresql/db2rtcl
RUN (cd /var/lib/postgresql/db2rtcl; tar xvf v11.5.8_linuxx64_rtcl.tar.gz)

USER postgres
WORKDIR /var/lib/postgresql
RUN (cd /var/lib/postgresql/db2rtcl/rtcl;   ./db2_install -y -f sysreq)

USER root
WORKDIR /

ENV CLASSPATH="/var/lib/postgresql/sqllib/java/db2java.zip:/var/lib/postgresql/sqllib/function:/var/lib/postgresql/sqllib/java/db2jcc_license_cu.jar:/var/lib/postgresql/sqllib/tools/clpplus.jar:/var/lib/postgresql/sqllib/tools/jline-0.9.93.jar:/var/lib/postgresql/sqllib/java/db2jcc4.jar:/var/lib/postgresql/sqllib/java/db2jcc_license_cisuz.jar:."
ENV DB2DIR="/var/lib/postgresql/sqllib"
ENV DB2INSTANCE="postgres"
ENV DB2LIB="/var/lib/postgresql/sqllib/lib"
ENV DB2_HOME="/var/lib/postgresql/sqllib"
ENV DB2_NET_CLIENT_PATH=""
ENV IBM_DB_DIR="/var/lib/postgresql/sqllib"
ENV IBM_DB_HOME="/var/lib/postgresql/sqllib"
ENV IBM_DB_INCLUDE="/var/lib/postgresql/sqllib/include"
ENV IBM_DB_LIB="/var/lib/postgresql/sqllib/lib"
ENV INSTHOME="/var/lib/postgresql"
ENV INST_DIR="/var/lib/postgresql/sqllib"
ENV LD_LIBRARY_PATH="/lib64:/usr/lib64:/lib/x86_64-linux-gnu:/var/lib/postgresql/sqllib/lib64:/var/lib/postgresql/sqllib/lib64/gskit:/var/lib/postgresql/sqllib/lib32"

ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#ENV PATH="$PATH:/usr/lib/jvm/java-8-openjdk-amd64/bin"
RUN ln -s /usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64/server/libjvm.so /usr/lib64/libjvm.so

EOF
