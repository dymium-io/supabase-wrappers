#!/bin/bash

PORT=9080

echo "Starting on port $PORT"
docker run --rm  --name db-analyzer.dymium.local \
       --network dymium \
       -p ${PORT}:8080 \
       db-analyzer \
       /main
