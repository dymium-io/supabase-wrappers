#!/usr/bin/env zsh

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

cat ${script_d}/Dockerfile.dev \
    | sed "s/@instantclient_version@/${instantclient_version}/g" \
    | docker build --platform linux/amd64 --compress -t db-dev -f - .
