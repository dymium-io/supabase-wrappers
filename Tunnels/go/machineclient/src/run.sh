#!/bin/bash
export LOG_LEVEL=Debug
export PORTAL=https://portal.dymium.local:3001/
export KEY=StsAuG6J8XXo
export SECRET=i0eIgzVnzURJmkwamtoqWcEvsLL9nrT1ah1Z82btArPyONGMbzltIV06bZUe6e8MdReh6gk0bY4McyI5ZxCP0T3afJ3MlNpmabrdaQbqildT1Ksdtv8MTc33ltKat4s8
export CUSTOMER=spoofcorp
export TUNNELSERVER=portal.dymium.local:15654
export LOCAL_ENVIRONMENT=true
export LISTENER_PORT=6666

./machineclient 2>&1 | tee output.txt

