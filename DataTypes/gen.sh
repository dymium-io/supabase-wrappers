#!/bin/bash

set -e

../bin/dataTypesGenerator -l golang -p ../DbAnalyzer/go/types -r types DbAnalyzer.hdtd
