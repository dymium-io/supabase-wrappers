FROM ubuntu:jammy
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Mesh Connector"
RUN apt update && apt install -y ca-certificates && rm -rf /var/cache/apk/*  
COPY myCA.pem /usr/local/share/ca-certificates/mozilla/myCA.crt
COPY myCA.pem /usr/share/ca-certificates/mozilla/myCA.crt
COPY myCA.pem /usr/local/share/ca-certificates/myCA.crt
COPY myCA.pem /usr/share/ca-certificates/myCA.crt
RUN update-ca-certificates --verbose --fresh
RUN apt install -y curl
COPY meshconnector /meshconnector
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/meshconnector"]
