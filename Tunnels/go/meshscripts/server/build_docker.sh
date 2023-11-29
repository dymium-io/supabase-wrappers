#!/bin/bash

set -e
set -x
build_d=BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

cd ../../meshserver
CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o ../meshscripts/server/$build_d/meshserver
        
retval=$?

[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}
cd ../meshscripts/server

docker build --compress -f Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t meshserver $build_d
