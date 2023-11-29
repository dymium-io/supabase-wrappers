FROM alpine:3.18.4
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Mesh Connector"
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*      
RUN apk add --update --no-cache openssl
RUN apk add curl
COPY meshconnector /meshconnector
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/meshconnector"]
