#!/bin/bash

SERVER_PORT=9099
AWS_LAMBDAS='{ "DbAnalyzer": "localhost:9080" }'

function run () {
    cd $d/../go
    go build

    set -x
    SERVER_PORT=$SERVER_PORT \
    AWS_LAMBDAS=$AWS_LAMBDAS \
    ./CheckLambda
}

function docker_run () {
    cd $d
    ./build_docker.sh
    set -x
    docker run --rm -e AWS_LAMBDAS="$AWS_LAMBDAS" -p $SERVER_PORT:80 \
	   check-lambda
}

d=$(dirname $0)
case "$1" in
    "" | "app")
	run
	;;
    "docker")
	AWS_LAMBDAS=$(echo -n $AWS_LAMBDAS | sed s/localhost/docker.for.mac.host.internal/g)
	docker_run
	;;
    *)
	echo "Usage: $0 [ app | docker ]"
	exit 255
	;;
esac
