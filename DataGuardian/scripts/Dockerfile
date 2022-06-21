FROM postgres:14.3

RUN apt update && apt install -y ca-certificates

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh

COPY postgres_fdw.so /usr/lib/postgresql/14/lib/postgres_fdw.so
RUN chmod 755 /usr/lib/postgresql/14/lib/postgres_fdw.so
