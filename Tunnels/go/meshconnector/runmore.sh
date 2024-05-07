#!/usr/bin/env bash

export LOG_LEVEL=Error
export PORTAL=https://portal.dymium.local:3001/
export CONNECTOR=df8b31b0-96d0-44d1-9007-2ff6002517a3
export KEY=eR2U69JpFILl
export SECRET=NvHbp3X1RQfpd7CXAVNBay6eUbvwuG-qnQBUbcHh5ac5vkte7FT-kg2s7-HFhw9L-Ir7-AzItMP0fEEmBVfqC7bSEwWzv4cA-URb7UXDuQKIloJISRK59Bc2KZTHqGlb
export CUSTOMER=spoofcorp
export TUNNELSERVER=portal.dymium.local:3009
export LOCAL_ENVIRONMENT=true
export HEALTHPORT=9912
export LOCAL_SEARCH=
go run --race connector.go
