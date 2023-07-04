#!/bin/zsh

fdws=(mysql_fdw postgres_fdw tds_fdw oracle_fdw)

(
    cd ../foreign_data_wrappers

    for f in ${fdws[@]}; do
	( cd $f; make USE_PGXS=true clean; )
    done
)

(
    cd ../obfuscator
    rm -rf target
)
