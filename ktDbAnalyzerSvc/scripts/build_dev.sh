#!/bin/zsh

set -e

script_d=$PWD/$(dirname $0)
build_d=${script_d}/BLD
setup_d=${script_d}/../../setup

KtDev=$(docker images kt-dev -q)
[ -z "$DbDev" ] || docker rmi -f "$DbDev"

cat <<EOF | docker build --platform linux/amd64 --compress -t kt-dev -f - .
FROM ubuntu:jammy


RUN apt-get update &&            \
    apt-get upgrade -y &&        \
    apt-get install -y           \
      build-essential            \
      openjdk-11-jdk             \
      curl wget unzip zip        \

#install gradle
RUN wget -c https://services.gradle.org/distributions/gradle-8.4-bin.zip -P /tmp
RUN unzip -d /opt/gradle /tmp/gradle-8.4-bin.zip


ENV GRADLE_HOME=/opt/gradle/gradle-8.4
ENV PATH="/opt/gradle/gradle-8.4/bin:/usr/local/go/bin:\${PATH}"
ENV GRADLE_USER_HOME=/build_cache

EOF
