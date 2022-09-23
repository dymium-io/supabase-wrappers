FROM postgres:14-alpine

RUN apk add ca-certificates \
  mysql-client mariadb-connector-c-dev \
  freetds

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh

COPY postgres_fdw.so mysql_fdw.so tds_fdw.so /usr/local/lib/postgresql/
RUN chmod 755 \
  /usr/local/lib/postgresql/postgres_fdw.so \
  /usr/local/lib/postgresql/mysql_fdw.so \
  /usr/local/lib/postgresql/tds_fdw.so

COPY \
  mysql_fdw.control mysql_fdw--1.1.sql \
  tds_fdw.control tds_fdw.sql \
  /usr/local/share/postgresql/extension/
