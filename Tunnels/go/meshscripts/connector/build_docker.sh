#!/bin/bash

build_d=BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

cd ../../meshconnector
CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o ../meshscripts/connector/$build_d/meshconnector
        
retval=$?

[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}
cd ../meshscripts/connector

docker build --compress -f Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t meshconnector $build_d
