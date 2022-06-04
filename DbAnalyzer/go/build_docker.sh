#!/bin/bash

project_d=$PWD/..
build_d=$PWD/BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -ldflags '-w -extldflags "-static"' -o "$build_d/main"

retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}

cd $build_d

cp ../entry.sh .

# creating docker
DbAnalyzer=$(docker images DbAnalyzer -q)
[ -z "$pdf2png" ] || docker rmi -f "$DbAnalyzer"

docker build --compress -f ../Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t DbAnalyzer .
