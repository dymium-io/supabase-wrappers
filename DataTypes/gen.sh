#!/bin/bash

set -e

if [ "$1" = "-c" ]
then
   shift
   CONF="$1"
   shift
else
   CONF=hdtd
fi

[ -z "$CONF" ] && {
   echo "Usage: $0 [-c <hdtd file>] [modules ...]"
   exit 255
}

cd $(dirname $0)
../bin/dataTypesGenerator $CONF "$@"
