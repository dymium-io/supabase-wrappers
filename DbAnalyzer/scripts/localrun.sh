#!/bin/bash

# localhost name as visible from within the docker:
# docker.for.mac.host.internal

docker run --rm  --name db-analyzer \
       -p 18080:8080 \
       db-analyzer \
       /main
