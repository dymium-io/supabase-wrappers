#!/usr/bin/env bash

for i in {1..100}; do
    curl -o /dev/null http://localhost:9090/big.mp4
done
