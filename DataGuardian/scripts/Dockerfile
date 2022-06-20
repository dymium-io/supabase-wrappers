FROM postgres:14.3

RUN apt update && apt install -y ca-certificates

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh
