#!/bin/bash

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
buildcache_d=${script_d}/../buildcache
setup_d=${script_d}/../../setup
ktDbAnalyzerSvc_d=${script_d}/../../ktDbAnalyzerSvc

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

[ -d $buildcache_d ] || {
    mkdir $buildcache_d
}

echo Building ktDbAnalyzerSvc...
docker run -it --rm \
                   -v $ktDbAnalyzerSvc_d:/src      \
                   -v $buildcache_d:/build_cache  \
                   kt-dev          \
                   /bin/sh -c      \
        "cd /src && \
         gradle build && \
         gradle distTar"

retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}


cd $build_d
# copy jar file to setup folder to add dependencies to DbGuardian
cp $ktDbAnalyzerSvc_d/build/distributions/ktDbAnalyzerSvc-0.0.1.tar ${setup_d}/ktDbAnalyzerSvc-0.0.1.tar
