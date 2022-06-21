#!/bin/bash

set -e

../bin/dataTypesGenerator -l golang -p ../DbAnalyzer/go/types -r DbAnalyzer/types DbAnalyzer.hdtd
../bin/dataTypesGenerator -l golang -p ../DbSync/go/types -r DbSync/types DbSync.hdtd
../bin/dataTypesGenerator -l golang -p ../DataGuardian/go/types -r DPostgres/types DbSync.hdtd

../bin/dataTypesGenerator -l golang -p ../web/go/src/types -r dymium/types web.hdtd

