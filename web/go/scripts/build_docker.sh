#!/bin/bash

build_d=BLD

[ -d $build_d ] && {
    rm -rf $build_d
}

mkdir $build_d
#mkdir -p ../assets/customer
#mkdir -p ../assets/admin

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
echo Build tunneling clients
cd ../../../Tunnels/go/client


CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1' -X 'main.ProtocolVersion=1' -w -extldflags "-static"' -o tunnel
chmod a+x tunnel
mkdir -p ../../../web/go/assets/customer/update/linux/amd64/
cp tunnel ../../../web/go/assets/customer/update/linux/amd64/
tar -zcvf tunnel.tar.gz tunnel
cp tunnel.tar.gz ../../../web/go/assets/customer/update/
mv tunnel.tar.gz ../../../web/js/packages/portal/public

CGO_ENABLED=0 GOOS=windows GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1' -X 'main.ProtocolVersion=1' -w -extldflags "-static"' -o tunnel.exe
mkdir -p ../../../web/go/assets/customer/update/windows/amd64/
cp tunnel ../../../web/go/assets/customer/update/windows/amd64/
zip tunnel_win.zip  tunnel.exe 
cp tunnel_win.zip ../../../web/go/assets/customer/
mv tunnel_win.zip ../../../web/js/packages/portal/public

CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=1' -X 'main.ProtocolVersion=1' -w -extldflags "-static"' -o tunnel
chmod a+x tunnel
mkdir -p ../../../web/go/assets/customer/update/darwin/amd64/
cp tunnel ../../../web/go/assets/customer/update/darwin/amd64/
zip tunnel_mac.zip  tunnel
cp tunnel_mac.zip ../../../web/go/assets/customer/
mv tunnel_mac.zip ../../../web/js/packages/portal/public

echo "Moved the client binaries"

cd ../../../web/go/scripts/

cp -r ../assets/admin $build_d/
cp -r ../assets/customer $build_d/

dymium=$(docker images dymium -q)
[ -z "$dymium" ] || docker rmi -f "$dymium"

docker build --compress -f Dockerfile \
       --label "git.branch=$(git branch --show-current)" \
       --label "git.commit=$(git rev-parse HEAD)" \
       -t dymium $build_d
