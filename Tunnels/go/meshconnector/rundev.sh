#!/bin/bash

export LOG_LEVEL=Debug
export HEALTHPORT=10101
export PORTAL=https://portal.dev.dymium.us/
export CONNECTOR=57150152-a641-4e81-b0a9-b8916c6f2890
export KEY=F52URLIoZYkW
export SECRET=edjQNUODroiiDItsaMVUcPwmJ5wM5GCQa3Oa5xE85LA13jXQxuFIcgbSvIDPQofcKC0f9mu6GUrSG5QkEr0wV0BI0cVe5wr-49uoGJMDzQTk9HBZ6piLNy08i-gSa1yw
export CUSTOMER=spoofcorp
export TUNNELSERVER=spoofcorp.dev.dymium.us:3009
export LOCAL_ENVIRONMENT=true

./meshconnector
