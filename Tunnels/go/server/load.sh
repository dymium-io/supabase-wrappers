#!/bin/bash

for i in {1..100}; do
    curl -o /dev/null http://localhost:24354/big.mp4
done
