#!/usr/bin/env bash

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

(
    cd ${script_d}/../go
    CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		  go build -a -ldflags '-w -extldflags "-static"' -o "${build_d}/main"
)

retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}

cd $build_d

# creating docker
CheckLambda=$(docker images check-lambda -q)
[ -z "$CheckLambda" ] || docker rmi -f "$CheckLambda"

docker build --compress -f ../Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t check-lambda .
