#!/bin/bash

go build -a -tags netgo -ldflags '-w -extldflags "-static"' -o dymium



