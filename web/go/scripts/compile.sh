#!/bin/bash

set -e

p=$PWD/$(dirname "$0")

cd $p/../..
for d in admin 
do
    cd $d
    yarn install
    cd ..
done

cd $p/../src
go build -o ../assets/server

cd $p
./build_docker.sh
