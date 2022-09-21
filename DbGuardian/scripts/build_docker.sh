#!/bin/bash

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD

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

fdw=':'
cp_args=''
for f in postgres_fdw mysql_fdw; do
    fdw="$fdw; cd /fdw/$f; make USE_PGXS=true"
    cp_args="$cp_args $f/$f.so $f/$f.control $f/$f--*.sql"
done
set -x
(
    cd $script_d/../foreign_data_wrappers
    docker run -it --rm -v $PWD:/fdw postgres-dev /bin/sh -c "$fdw"
    eval cp "$cp_args $build_d"
)


cd $build_d

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

docker build --compress -f ../Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t data-guardian .
