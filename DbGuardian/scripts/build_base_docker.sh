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

#DB2 libs
cp ${setup_d}/db2/v11.5.8_linuxx64_rtcl.tar.gz .
#cp ${setup_d}/db2/odbcinst.ini .
cp ${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz .


fdws=(postgres_fdw mysql_fdw tds_fdw oracle_fdw db2_fdw)
(
	cd $script_d/../foreign_data_wrappers
	for f in ${fdws[@]}; do
	    docker run -it --rm \
		   -v $PWD:/fdw \
		   postgres-dev \
		   /bin/sh -c \
		   "cd /fdw/$f; make USE_PGXS=true; DESTDIR=/fdw make USE_PGXS=true install"
	done
	docker run -it --rm \
		   -e USER_ID=$UID \
		   -e GROUP_ID=$GID \
		   -v $PWD:/fdw \
		   postgres-dev \
		   /bin/sh -c \
		   "cd /fdw; tar czv --owner=root --group=root -f usr.tar.gz usr; rm -rf usr"
)
(
	cd $script_d/../foreign_data_wrappers/mongo_fdw
  docker run -it --rm \
		   -v $PWD:/fdw \
		   -e MONGO_FDW_SOURCE_DIR=/fdw \
		   -e MONGOC_INSTALL_DIR=/fdw/mongo-c-driver \
		   -e JSONC_INSTALL_DIR=/fdw/json-c \
		   -e PKG_CONFIG_PATH=/fdw/mongo-c-driver/src/libmongoc/src:/fdw/mongo-c-driver/src/libbson/src \
		   postgres-dev \
		   /bin/bash -c \
		   "cd /fdw; (./autogen.sh --with-master; \
                    make USE_PGXS=true; \
                    DESTDIR=. make USE_PGXS=true install) && \
                    tar czv --owner=root --group=root -f mongo.tar.gz usr; rm -rf usr && \
                    mkdir -p usr/local/libmongo; cp json-c/lib/* usr/local/libmongo; \
                    cp mongo-c-driver/lib/* usr/local/libmongo; \
                    tar czv --owner=root --group=root -f mongo-lib.tar.gz usr; rm -rf lib; rm -rf json-c"
)
(
	cd $script_d/../foreign_data_wrappers/jdbc_fdw
  docker run -it --rm \
		   -v $PWD:/fdw \
		   postgres-dev \
		   /bin/bash -c \
		      "cd /fdw; \
          make USE_PGXS=true clean; \
          DESTDIR=. make USE_PGXS=true install && \
          cp  /usr/lib/postgresql/14/lib/JDBC* usr/lib/postgresql/14/lib/ && \
          cp /usr/lib/postgresql/14/lib/resultSetInfo.class usr/lib/postgresql/14/lib/ &&\
          tar czv --owner=root --group=root -f jdbc_fdw.tar.gz usr; rm -rf usr"
)


(
    cd $script_d/../obfuscator
    docker run -it --rm \
		   -v $PWD:/obfuscator \
		   postgres-dev \
		   /bin/sh -c \
		   "cd /obfuscator; ( cargo pgx package --out-dir .  2>&1 | sed -u '/jemalloc/d' ) && \
                    tar czv --owner=root --group=root -f obfuscator.tar.gz usr; rm -rf usr"
)


mv ../../foreign_data_wrappers/usr.tar.gz .
mv ../../obfuscator/obfuscator.tar.gz .
mv ../../foreign_data_wrappers/mongo_fdw/mongo.tar.gz .
mv ../../foreign_data_wrappers/mongo_fdw/mongo-lib.tar.gz .
mv ../../foreign_data_wrappers/jdbc_fdw/jdbc_fdw.tar.gz .



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
    ksh unzip                    \
    openjdk-8-jdk              \
    libaio1 &&                   \
  ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so.21 /usr/lib/x86_64-linux-gnu/libmysqlclient.so && \
  mkdir -p /opt/oracle


COPY usr.tar.gz obfuscator.tar.gz mongo.tar.gz mongo-lib.tar.gz jdbc_fdw.tar.gz /
RUN tar xzvf /usr.tar.gz && rm /usr.tar.gz
RUN tar xzvf /obfuscator.tar.gz && rm /obfuscator.tar.gz
RUN tar xzvf /mongo.tar.gz && rm /mongo.tar.gz
RUN tar xzvf /mongo-lib.tar.gz && rm /mongo-lib.tar.gz
RUN tar xzvf /jdbc_fdw.tar.gz && rm /jdbc_fdw.tar.gz

COPY ${instantclient_version}/ /opt/oracle/${instantclient_version}/
RUN echo /opt/oracle/${instantclient_version} > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

RUN mkdir -p /var/lib/postgresql/db2rtcl
COPY v11.5.8_linuxx64_rtcl.tar.gz /var/lib/postgresql/db2rtcl
RUN (cd /var/lib/postgresql/db2rtcl; tar xvf v11.5.8_linuxx64_rtcl.tar.gz)

USER postgres
WORKDIR /var/lib/postgresql
RUN (cd /var/lib/postgresql/db2rtcl/rtcl;   ./db2_install -y -f sysreq)

USER root
WORKDIR /

ENV CLASSPATH="/var/lib/postgresql/sqllib/java/db2java.zip:/var/lib/postgresql/sqllib/function:/var/lib/postgresql/sqllib/java/db2jcc_license_cu.jar:/var/lib/postgresql/sqllib/tools/clpplus.jar:/var/lib/postgresql/sqllib/tools/jline-0.9.93.jar:/var/lib/postgresql/sqllib/java/db2jcc4.jar:/var/lib/postgresql/sqllib/java/db2jcc_license_cisuz.jar:."
ENV DB2DIR="/var/lib/postgresql/sqllib"
ENV DB2INSTANCE="postgres"
ENV DB2LIB="/var/lib/postgresql/sqllib/lib"
ENV DB2_HOME="/var/lib/postgresql/sqllib"
ENV DB2_NET_CLIENT_PATH=""
ENV IBM_DB_DIR="/var/lib/postgresql/sqllib"
ENV IBM_DB_HOME="/var/lib/postgresql/sqllib"
ENV IBM_DB_INCLUDE="/var/lib/postgresql/sqllib/include"
ENV IBM_DB_LIB="/var/lib/postgresql/sqllib/lib"
ENV INSTHOME="/var/lib/postgresql"
ENV INST_DIR="/var/lib/postgresql/sqllib"
ENV LD_LIBRARY_PATH="/lib64:/usr/lib64:/lib/x86_64-linux-gnu:/var/lib/postgresql/sqllib/lib64:/var/lib/postgresql/sqllib/lib64/gskit:/var/lib/postgresql/sqllib/lib32"

ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#ENV PATH="$PATH:/usr/lib/jvm/java-8-openjdk-amd64/bin"
RUN ln -s /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/libjvm.so /usr/lib64/libjvm.so

EOF
