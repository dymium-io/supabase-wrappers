#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
setup_d=${script_d}/../../setup
wrk=${script_d}/.wrk

[ -d "$script_d" ] || {
    echo "Derectory $script_d does not exist"
    exit 255
}

[ -d "$wrk" ] && {
    rm -rf $wrk
}

mkdir $wrk
trap "rm -rf $wrk" EXIT
cd $wrk

instantclient_version="$(${setup_d}/oracle.sh version linux)"
instantclients_all=($(${setup_d}/oracle.sh packages linux))

declare -a instantclients
for c in ${instantclients_all[@]}; do
	[[ $c == *basic* ]] && {
		instantclients+=($c)
	}
done

for f in ${instantclients[@]}; do
	[ -f ${setup_d}/oracle/$f ] || {
		echo "Oracle instantclient file $f not found in ${setup_d}"
		echo "Please use ${setup_d}/oracle.sh script to get them"
		exit -1
	}
done

for f in ${instantclients[@]}; do
	unzip ${setup_d}/oracle/$f
done

# creating docker
GuardianBase=$(docker images guardian-base -q)
[ -z "$GuardianBase" ] || docker rmi -f "$GuardianBase"

cat <<EOF | docker build --platform linux/amd64 --compress -t guardian-base -f - .
FROM ubuntu/postgres

RUN apt update &&                \
  apt upgrade -y &&              \
  apt install -y ca-certificates \
    libmariadb3 libmariadb-dev   \
    libmysqlclient21             \
    freetds-bin                  \
    libaio1 &&                   \
  ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so.21 /usr/lib/x86_64-linux-gnu/libmysqlclient.so && \
  mkdir -p /opt/oracle

COPY ${instantclient_version}/ /opt/oracle/${instantclient_version}/
RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

EOF
