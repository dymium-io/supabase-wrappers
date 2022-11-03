FROM ubuntu/postgres

RUN apt update &&                \
  apt upgrade -y &&              \
  apt install -y ca-certificates \
    libmariadb3 libmariadb-dev   \
    freetds-bin                  \
    libaio1 &&                   \
  mkdir -p /opt/oracle

COPY ./instantclient_21_8/ /opt/oracle/instantclient_21_8/
RUN echo /opt/oracle/instantclient_21_8 > /etc/ld.so.conf.d/oracle-instantclient.conf && \
    ldconfig

COPY initializer /docker-entrypoint-initdb.d/initializer.sh
RUN chmod 777 /docker-entrypoint-initdb.d/initializer.sh

COPY usr.tar.gz /
RUN tar xzvf /usr.tar.gz && rm /usr.tar.gz
