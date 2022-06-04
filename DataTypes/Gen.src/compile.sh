#!/bin/sh

set -e

case ${1:-"--darwin"} in
    "--all")
	$0 --darwin
	$0 --linux
	;;
    "--darwin")
	stack build --copy-bins --local-bin-path darwin
	upx --best darwin/Gen-exe
	;;
    "--linux")
	cat <<EOF | docker run --rm -i -v $HOME/.stack:/root/.stack -v $PWD:/z fpco/stack-build-small:lts /bin/bash -
apt update
apt upgrade -y
apt install -y upx
cd /z
stack build --copy-bins --local-bin-path linux
upx --best linux/Gen-exe
EOF
	retval=$?
	[ $retval -eq 0 ] || {
	    echo "build failed with error code $retval"
	    exit $retval
	}
	;;
    *)
	echo "Usage: ./compile.sh [--darwin|--linux|--all]"
	exit 255
	;;
esac
