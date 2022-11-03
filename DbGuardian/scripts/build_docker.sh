#!/bin/bash

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

instantclient_version='instantclient_21_8'
instantclients=('instantclient-basic-linux.x64-21.8.0.0.0dbru.zip')
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

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

docker build --compress -f ../Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t data-guardian .
