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

build_d=$build_d/bld
mkdir -p $build_d
cd $build_d

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

#DB2 libs
cp ${setup_d}/db2/v11.5.8_linuxx64_rtcl.tar.gz ${build_d}
unzip ${setup_d}/db2/db2_home.zip

PostgresDev=$(docker images postgres-dev -q)
[ -z "$PostgresDev" ] || docker rmi -f "$PostgresDev"

cat ${script_d}/Dockerfile.dev \
    | sed "s/@instantclient_version@/${instantclient_version}/g" \
    | docker build --platform linux/amd64 --compress -t postgres-dev-tmp -f - .
docker run --name postgres-dev-tmp \
       -v $build_d/../target:/root/.cargo/target \
       postgres-dev-tmp \
       /bin/bash -c "\
              echo CARGO_TARGET_DIR=\$CARGO_TARGET_DIR; \
              cargo install cargo-pgrx --version 0.11.2 2>&1 | sed -u '/jemalloc/d' && \
              cargo pgrx init --pg14 /usr/bin/pg_config 2>&1 | sed -u '/jemalloc/d'; \
        "
docker commit postgres-dev-tmp postgres-dev
docker rm postgres-dev-tmp
docker rmi postgres-dev-tmp
