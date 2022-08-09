#!/bin/sh

cat <<\EOF | docker build -t postgres-dev -
FROM ubuntu:latest

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y build-essential \
      postgresql-server-dev-all \
      libmysqlclient-dev
