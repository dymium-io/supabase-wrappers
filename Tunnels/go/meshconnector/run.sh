#!/bin/bash

export LOG_LEVEL=Debug
export PORTAL=https://portal.dymium.local:3001/
export CONNECTOR=4a87fb02-f76c-4a0d-82b1-cfdcc23bdb6b
export KEY=QtWHYSxMFeXG
export SECRET=Ec0HnhNm4UtwNrKBFZC69ptQMD9UwNQF7Ngh-OnUxUfJGL3L8pNXQreZDA10g2hdlyLX8QTkLWv0PhOkXrkn9J7fL2tILvbxGABKvaVU0NzT6ZAY0sp2yQxJAXezhovp
export CUSTOMER=spoofcorp
export TUNNELSERVER=portal1.dymium.local:3009
export LOCAL_ENVIRONMENT=true
export HEALTHPORT=9911
export LOCAL_SEARCH=
./meshconnector 2>&1 | tee output.txt

#go run --race connector.go
