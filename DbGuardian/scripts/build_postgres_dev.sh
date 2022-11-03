#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

instantclient_version='instantclient_21_8'
instantclients=('instantclient-basic-linux.x64-21.8.0.0.0dbru.zip' \
		    'instantclient-sdk-linux.x64-21.8.0.0.0dbru.zip')
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

cat <<EOF | docker build -t postgres-dev -f - .
FROM ubuntu:latest


RUN apt-get update &&            \
    apt-get upgrade -y &&        \
    apt-get install -y           \
      build-essential            \
      postgresql-server-dev-all  \
      libmariadb3 libmariadb-dev \
      freetds-dev                \
      libaio1 &&                 \
    mkdir -p /opt/oracle

COPY ./ /opt/oracle/

RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

ENV ORACLE_HOME=/opt/oracle/${instantclient_version}
EOF
