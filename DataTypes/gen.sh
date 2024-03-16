#!/usr/bin/env bash

set -e

if [ "$1" = "-h" -o "$1" = "--help" ]
then
   echo "Usage: $0 [-c <hdtd file>] [modules ...]"
   exit 
elif [ "$1" = "-c" ]
then
   shift
   CONF="$1"
   [ -z "$CONF" ] && {
      echo "Usage: $0 [-c <hdtd file>] [modules ...]"
      exit 255
   }
   shift
else
   CONF=hdtd
fi

cd $(dirname $0)
../bin/dataTypesGenerator $CONF "$@"
