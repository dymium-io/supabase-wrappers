#!/usr/bin/env bash

build_d=BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

cd ../src
CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o ../scripts/$build_d/
        
retval=$?

[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}
cd ../scripts

docker build --compress -f Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t machinetunnel $build_d
