#!/usr/bin/env zsh

set -e
script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

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


# build log collector
lc_build_d=${script_d}/../../logcollector/scripts/BLD

[ -d $lc_build_d ] && {
	rm -rf $lc_build_d
}

mkdir $lc_build_d

(
	cd ${lc_build_d}/../../go
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		go build -a -ldflags '-w -extldflags "-static"' -o "${lc_build_d}/logcollector"
)

retval=$?
[ $retval -ne 0 ] && {
	echo "build failed with error code $retval"
	exit $retval
}
cp ${lc_build_d}/logcollector ${build_d}/logcollector
cp $build_d/../startup.sh $build_d/startup.sh
cd $build_d

#get jdbc dependencies
jdbclibs=${setup_d}/ktDbAnalyzerSvc-0.0.1.tar
[ -f $jdbclibs ] || {
		echo "jdbc lib dependencies file not found in ${setup_d}"
		echo "Please build ktDbAnalyzerSvc to get them"
		exit -1
	}
cp $jdbclibs .

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

cp ../sqlnet.ora .

cat ${script_d}/Dockerfile \
    | sed "s/@instantclient_version@/${instantclient_version}/g" \
    | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t data-guardian -f - .
