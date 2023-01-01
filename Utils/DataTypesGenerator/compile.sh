#!/bin/sh

set -e

case ${1:-"--darwin"} in
    "--all")
	$0 --darwin
	$0 --linux
	;;
    "--darwin")
	stack build --copy-bins --local-bin-path darwin
	zstd -f --rm --ultra darwin/Gen-exe -o ../../bin/darwin/dataTypesGenerator.zst
	;;
    "--linux")
	cat <<EOF | docker run --rm -i -v $HOME/.stack:/root/.stack -v $PWD:/z fpco/stack-build-small:lts /bin/bash -
cd /z
stack build --copy-bins --local-bin-path linux
EOF
	retval=$?
	[ $retval -eq 0 ] || {
	    echo "build failed with error code $retval"
	    exit $retval
	}
	zstd -f --rm --ultra linux/Gen-exe -o ../../bin/linux/dataTypesGenerator.zst
	;;
    *)
	echo "Usage: ./compile.sh [--darwin|--linux|--all]"
	exit 255
	;;
esac
