#!/usr/bin/env bash

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup
ktDbAnalyzerSvc_d=${script_d}/../../ktDbAnalyzerSvc

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d

instantclient_version="$(${setup_d}/oracle.sh version linux)"
instantclients=($(${setup_d}/oracle.sh packages linux))

for f in ${instantclients[@]}; do
	[ -f ${setup_d}/oracle/$f ] || {
		echo "Oracle instantclient file $f not found in ${setup_d}"
		echo "Please use ${setup_d}/oracle.sh script to get them"
		exit -1
	}
done

cd $build_d

ktDbAnalyzerSvc=${setup_d}/ktDbAnalyzerSvc-0.0.1.tar
[ -f $ktDbAnalyzerSvc ] || {
		echo "ktDbAnalyzerSvc file not found in ${setup_d}"
		echo "Please build ktDbAnalyzerSvc."
		exit -1
	}
cp $ktDbAnalyzerSvc .

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

#DB2 libs
unzip ${setup_d}/db2/db2_home.zip
cp ${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz ${build_d}

echo Building DbAnalyzer...
tar czf DbAnalyzer.tgz -C ../../.. DbAnalyzer/go
tar czf libs.tgz -C ../../.. libs/go
docker run -it --rm \
                   -v ./:/src      \
                   -v $script_d/.go/:/go/ \
                   db-dev          \
                   /bin/sh -c      \
		   "cd /src && \
                    tar --warning=no-unknown-keyword -xf DbAnalyzer.tgz && \
                    tar --warning=no-unknown-keyword -xf libs.tgz && \
                    cd DbAnalyzer/go &&\
                    GOOS=linux GOARCH=amd64 \
                    go build -buildvcs=false -a -ldflags '-w ' -o /src/main"

retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}

cd $build_d

cp ../entry.sh .
cp ../sqlnet.ora .


# creating docker
DbAnalyzer=$(docker images db-analyzer -q)
[ -z "$DbAnalyzer" ] || docker rmi -f "$DbAnalyzer"


cat ${script_d}/Dockerfile \
    | sed "s/@instantclient_version@/${instantclient_version}/g" \
    | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t db-analyzer -f - .
