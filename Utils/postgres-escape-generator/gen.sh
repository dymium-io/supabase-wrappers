#!/usr/bin/env bash

set -e

n="postgres_escape.go"
tmp="/tmp/$n"
dests=("DbSync/go" "DbGuardian/go")

go run ./gen-escaper.go > $tmp
[ $? = 0 ] || {
    echo "Generator returned error"
    exit 255
}

set -x
for dest in ${dests[@]}; do
    cp $tmp "../../$dest"
done

rm $tmp
