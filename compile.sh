#!/bin/sh

set -e

case ${1:-"--darwin"} in
    "--all")
	$0 --darwin
	$0 --linux
	;;
    "--darwin")
	if [ "$(arch)" = "arm64" ]
	then
	    add_path=/opt/homebrew/bin
	    install_path=../../bin/darwin/arm64/mallard.zst
	else
	    add_path=/usr/local/bin
	    install_path=../../bin/darwin/x86_64/mallard.zst
	fi
	PATH="${add_path}:$PATH"
	stack build --copy-bins --local-bin-path darwin
        zstd -f --rm --ultra darwin/mallard -o ${install_path}
	;;
    "--linux")
	cat <<EOF | docker run --rm -i -v $HOME/.stack:/root/.stack -v $PWD:/z fpco/stack-build-small:lts-21.19 /bin/bash -
apt update
apt upgrade -y
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends tzdata
printf 'tzdata tzdata/Areas select US\ntzdata tzdata/Zones/US select LosAngeles\n' | debconf-set-selections
rm /etc/timezone
rm /etc/localtime
dpkg-reconfigure -f noninteractive tzdata
apt install -y libpq-dev
cd /z
stack setup --install-ghc
stack build --copy-bins --local-bin-path linux
EOF
	retval=$?
	[ $retval -eq 0 ] || {
	    echo "build failed with error code $retval"
	    exit $retval
	}
        zstd -f --rm --ultra linux/mallard -o ../../bin/linux/mallard.zst
	;;
    *)
	echo "Usage: ./compile.sh [--darwin|--linux|--all]"
	exit 255
	;;
esac
