#!/bin/bash

build_d=BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d
mkdir -p ../assets/customer
mkdir -p ../assets/admin

cd ../src
CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o ../scripts/$build_d/server
        
retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}
./test.sh
retval=$?
[ $retval -ne 0 ] && {
    echo "unit test failed with error code $retval"
    exit $retval
}

echo Build main app

cd ../../js/packages/admin/
yarn run build
retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}

cd ../portal
yarn run build
retval=$?
[ $retval -ne 0 ] && {
    echo "build failed with error code $retval"
    exit $retval
}
yarn test --silent
retval=$?
[ $retval -ne 0 ] && {
    echo "jest failed with error code $retval"
    exit $retval
}

cd ../../../go/scripts

cp -r ../assets/admin $build_d/
cp -r ../assets/customer $build_d/

dymium=$(docker images dymium -q)
[ -z "$dymium" ] || docker rmi -f "$dymium"

docker build --compress -f Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t dymium $build_d
