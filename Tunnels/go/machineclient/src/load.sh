#!/bin/bash

for i in {1..100}; do
    curl -o /dev/null http://localhost:6666/big.bin
done
