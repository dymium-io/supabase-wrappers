#!/bin/bash

for i in {1..100}; do
    curl -o /dev/null http://localhost:30001/big.bin
done
