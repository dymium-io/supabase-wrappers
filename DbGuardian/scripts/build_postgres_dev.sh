#!/bin/sh

cat <<\EOF | docker build -t postgres-dev -
FROM alpine:3.16

RUN apk add build-base postgresql14-dev mysql-client mariadb-connector-c-dev
EOF
