FROM alpine:3
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Mesh Connector"
     
RUN apk update && \
      apk upgrade --no-cache && \
      apk add --no-cache ca-certificates openssl curl && \
      rm -rf /var/cache/apk/*

RUN apk add --update --no-cache openssl
RUN apk add curl
COPY meshconnector /meshconnector
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/meshconnector"]
