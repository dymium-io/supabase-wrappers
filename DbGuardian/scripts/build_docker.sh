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

(
    cd $script_d/../postgres/postgres_fdw
    docker run -it --rm -v $PWD:/postgres_fdw postgres-dev /bin/bash -c "cd /postgres_fdw; USE_PGXS=true make"
)


cd $build_d
cp $script_d/../postgres/postgres_fdw/postgres_fdw.so .

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

docker build --compress -f ../Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t data-guardian .
