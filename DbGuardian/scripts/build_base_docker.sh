#!/usr/bin/env zsh

set -e

script_d=$PWD/$(dirname $0)
setup_d=${script_d}/../../setup
build_d=$script_d/BLD/bld
target_d=$script_d/BLD/target

[ -d "$script_d" ] || {
    echo "Directory $script_d does not exist"
    exit 255
}

[ -d "$build_d" ] || {
    echo "Directory $build_d does not exist"
    echo "You should run $script_d/build_postgres_dev.sh before running this script"
    exit 255
}

force_rebuild="true"

main() {
    case "$1" in
        "") create_docker ;;
        finalize)
            force_rebuild="false"
            create_docker
            ;;
        postgres) build_fdw postgres_fdw ;;
        oracle)   build_fdw oracle_fdw ;;
        tds)      build_fdw tds_fdw ;;
        db2)      build_db2 ;;
        mysql)    build_fdw mysql_fdw ;;
        supabase) build_supabase_wrappers ;;
        mongo)    build_mongo ;;
        jdbc)     build_jdbc ;;
        pgsodium) build_pgsodium ;;
        vault)    build_fdw vault ;;
        obfuscator) build_obfuscator ;;
        *)
            echo "Usage: $0 [ postgres | oracle | tds | db2 | mysql | supabase | mongo | jdbc | obfuscator | pgsodium | vault ]"
            exit 0
            ;;
    esac
}

build_oracle() {

    local old_dir="$(pwd)"
    cd $build_d

    local instantclient_version="$(${setup_d}/oracle.sh version linux)"
    local instantclients_all=($(${setup_d}/oracle.sh packages linux))

    local instantclients=()
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

    build_fdw oracle_fdw

    cd $old_dir
}

build_db2() {

    local old_dir="$(pwd)"

    rm -rf $build_d/db2
    mkdir $build_d/db2
    cd $build_d/db2

    local rtcllibs=${setup_d}/db2/v11.5.8_linuxx64_rtcl.tar.gz
    [ -f $rtcllibs ] || {
       echo "DB2 rtcl lib dependencies file not found in ${setup_d}"
       echo "Please get them from S3"
       exit -1
    }
    tar xvf $rtcllibs

    # local odbclibs=${setup_d}/db2/v11.5.4_linuxx64_odbc_cli.tar.gz
    # [ -f $rtcllibs ] || {
    #   echo "DB2 odbc lib dependencies file not found in ${setup_d}"
    #   echo "Please get them from S3"
    #   exit -1
    # }
    # cp $odbclibs .

    build_fdw db2_fdw

    cd $old_dir
}

build_supabase_wrappers() {
    local old_dir="$(pwd)"
    cd $script_d/../foreign_data_wrappers
    docker run -it --rm \
        -v $PWD:/fdw \
	-v $build_d/supabase_wrappers:/dst \
        -v $target_d:/root/.cargo/target \
        postgres-dev \
        /bin/bash -c "\
	  cd /fdw/supabase_wrappers/wrappers; \
          cargo pgrx package --out-dir /dst --features pg14,all_fdws\
        "
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_supabase_wrappers failed"
        exit 255
    }
    cd $old_dir
}

build_pgsodium() {
    local fdw=pgsodium
    local old_dir="$(pwd)"
    cd $script_d/../foreign_data_wrappers
    docker run --rm \
        -v $PWD:/fdw \
	-v $build_d/$fdw:/dst \
        postgres-dev \
        /bin/sh -c "\
           set -x; \
           cd /fdw/$fdw && \
           make USE_PGXS=true && \
           DESTDIR=/dst make USE_PGXS=true install && \
           cp getkey_scripts/pgsodium_getkey_urandom.sh /dst/pgsodium_getkey
        "
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_fdw $fdw failed"
        exit 255
    }
    cd $old_dir
}


build_mongo() {
    local old_dir="$(pwd)"
    cd $script_d/../foreign_data_wrappers/mongo_fdw
    docker run -it --rm \
        -v $PWD:/fdw \
	-v $build_d/mongo_fdw:/dst \
	-v $build_d/mongo_lib:/mongo-lib \
        -e MONGO_FDW_SOURCE_DIR=/fdw \
        -e MONGOC_INSTALL_DIR=/mongo-lib/mongo-c-driver \
        -e JSONC_INSTALL_DIR=/mongo-lib/json-c \
        -e PKG_CONFIG_PATH=/mongo-lib/mongo-c-driver/lib/pkgconfig:/mongo-lib/json-c/lib/pkgconfig \
        postgres-dev \
        /bin/bash -c \
        "cd /fdw; \
        ( \
           ./autogen.sh --with-master; \
           make USE_PGXS=true; \
           DESTDIR=/dst make USE_PGXS=true install \
	)"
        # && \
        # tar czv --owner=root --group=root -f mongo.tar.gz usr; rm -rf usr && \
        # mkdir -p usr/local/libmongo && \
        # cp -r json-c/lib/* usr/local/libmongo && \
        # cp -r mongo-c-driver/lib/* usr/local/libmongo && \
        # tar czv --owner=root --group=root -f mongo-lib.tar.gz usr && \
        # rm -rf lib json-c usr"
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_mongo failed"
        exit 255
    }
    cd $old_dir
}

build_jdbc() {
    local old_dir="$(pwd)"
    cd $script_d/../foreign_data_wrappers/jdbc_fdw
    docker run -it --rm \
        -v $PWD:/fdw \
	-v $build_d/jdbc_fdw:/dst \
        postgres-dev \
        /bin/bash -c \
        "cd /fdw; \
         make USE_PGXS=true clean && \
         DESTDIR=/dst make USE_PGXS=true install && \
         cp /usr/lib/postgresql/14/lib/JDBC* /dst/usr/lib/postgresql/14/lib/ && \
         cp /usr/lib/postgresql/14/lib/resultSetInfo.class /dst/usr/lib/postgresql/14/lib/"
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_jdbc failed"
        exit 255
    }
    cd $old_dir
}


build_obfuscator() {
    local old_dir="$(pwd)"
    cd $script_d/../obfuscator
    docker run -it --rm \
        -v $PWD:/obfuscator \
        -v $build_d/obfuscator:/dst \
        -v $target_d:/root/.cargo/target \
        postgres-dev \
        /bin/sh -c "\
        cd /obfuscator && cargo pgrx package --out-dir /dst  2>&1 | sed -u '/jemalloc/d'"
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_obfuscator failed"
        exit 255
    }
    cd $old_dir
}

build_fdw() {
    local fdw=$1
    local old_dir="$(pwd)"
    cd $script_d/../foreign_data_wrappers
    docker run --rm \
        -v $PWD:/fdw \
	-v $build_d/$fdw:/dst \
        postgres-dev \
        /bin/sh -c "\
           cd /fdw/$fdw && \
           make USE_PGXS=true && \
           DESTDIR=/dst make USE_PGXS=true install
        "
    exit_status=$?
    [ $exit_status -eq 0 ] || {
        echo "build_fdw $fdw failed"
        exit 255
    }
    cd $old_dir
}

create_docker() {
    run_builds
    make_docker
}

run_builds() {
    [ "$force_rebuild" = "true" -o ! -d $build_d/postgres_fdw ] && build_fdw postgres_fdw
    [ "$force_rebuild" = "true" -o ! -d $build_d/oracle_fdw ] && build_fdw oracle_fdw
    [ "$force_rebuild" = "true" -o ! -d $build_d/tds_fdw ] && build_fdw tds_fdw
    [ "$force_rebuild" = "true" -o ! -d $build_d/db2_fdw ] && build_db2
    [ "$force_rebuild" = "true" -o ! -d $build_d/mysql_fdw ] && build_fdw mysql_fdw
    [ "$force_rebuild" = "true" -o ! -d $build_d/supabase_wrappers ] && build_supabase_wrappers
    [ "$force_rebuild" = "true" -o ! -d $build_d/mongo_fdw ] && build_mongo
    [ "$force_rebuild" = "true" -o ! -d $build_d/jdbc_fdw ] && build_jdbc
    [ "$force_rebuild" = "true" -o ! -d $build_d/pgsodium ] && build_pgsodium
    [ "$force_rebuild" = "true" -o ! -d $build_d/vault ] && build_fdw vault
    [ "$force_rebuild" = "true" -o ! -d $build_d/obfuscator ] && build_obfuscator
    return 0
}

make_docker() {
    # creating docker
    GuardianBase=$(docker images guardian-base -q)
    [ -z "$GuardianBase" ] || docker rmi -f "$GuardianBase"

    local instantclient_version="$(${setup_d}/oracle.sh version linux)"
    [ -z "$instantclient_version" ] && {
        echo "\'\$(${setup_d}/oracle.sh version linux)\' failed to return instantclient_version"
        exit 255
    }

    (
        cd $build_d
        cat ${script_d}/Dockerfile.base \
	    | sed "s/@instantclient_version@/${instantclient_version}/g" \
	    | docker build --platform linux/amd64 --compress -t guardian-base -f - .
    )
}

main "$@"
