#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

[ -d $build_d ] && {
	rm -rf $build_d
}

mkdir $build_d

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
                    mkdir -p usr/local/libmongo; cp json-c/lib/* usr/local/libmongo; cp mongo-c-driver/lib/* usr/local/libmongo; tar czv --owner=root --group=root -f mongo-lib.tar.gz usr; rm -rf lib"
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
          tar czv --owner=root --group=root -f jdbc_fdw.tar.gz usr" #; rm -rf usr"
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

cd $build_d
mv ../../foreign_data_wrappers/usr.tar.gz .
mv ../../obfuscator/obfuscator.tar.gz .
mv ../../foreign_data_wrappers/mongo_fdw/mongo.tar.gz .
mv ../../foreign_data_wrappers/mongo_fdw/mongo-lib.tar.gz .
mv ../../foreign_data_wrappers/jdbc_fdw/jdbc_fdw.tar.gz .

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

# creating docker
DataGuardian=$(docker images data-guardian -q)
[ -z "$DataGuardian" ] || docker rmi -f "$DataGuardian"

cat <<EOF | docker build --platform linux/amd64 --compress --label "git.branch=$(git branch --show-current)" --label "git.commit=$(git rev-parse HEAD)" -t data-guardian -f - .
FROM guardian-base

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh

COPY usr.tar.gz obfuscator.tar.gz mongo.tar.gz mongo-lib.tar.gz jdbc_fdw.tar.gz /
RUN tar xzvf /usr.tar.gz && rm /usr.tar.gz
RUN tar xzvf /obfuscator.tar.gz && rm /obfuscator.tar.gz
RUN tar xzvf /mongo.tar.gz && rm /mongo.tar.gz
RUN tar xzvf /mongo-lib.tar.gz && rm /mongo-lib.tar.gz
RUN tar xzvf /jdbc_fdw.tar.gz #&& rm /jdbc_fdw.tar.gz

# Add logcollector
RUN addgroup nobody tty && addgroup postgres tty

# forward postgres logs to docker log collector
RUN mkdir -p /var/log/postgres && chown postgres:postgres /var/log/postgres && chmod a+rwx /var/log/postgres && \
mkdir -p /tmp && mkfifo /tmp/logpipe && chown postgres:postgres /tmp/logpipe && chmod 0644 /tmp/logpipe && \
mkfifo /tmp/errpipe && chown postgres:postgres /tmp/errpipe && chmod 0644 /tmp/errpipe

RUN ln -sf /tmp/logpipe /var/log/postgres/postgres.csv && ln -sf /tmp/errpipe /var/log/postgres/postgres.log

COPY ./logcollector /usr/local/bin/logcollector
RUN chmod a+x /usr/local/bin/logcollector

COPY ./startup.sh /usr/local/bin/startup.sh
RUN chmod a+x /usr/local/bin/startup.sh

ENV LD_LIBRARY_PATH="/usr/local/libmongo:/lib64:/usr/lib64:/lib/x86_64-linux-gnu:/var/lib/postgresql/sqllib/lib64:/var/lib/postgresql/sqllib/lib64/gskit:/var/lib/postgresql/sqllib/lib32"

RUN echo "en_US.UTF-8 UTF-8"> /etc/locale.gen
RUN locale-gen

ENTRYPOINT ["/usr/local/bin/startup.sh"]

EOF
