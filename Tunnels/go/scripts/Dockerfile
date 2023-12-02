FROM alpine:3.18.4
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Tunneling Server"
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*      
RUN apk add --update --no-cache openssl
RUN apk add curl
COPY server /server
EXPOSE 80 443
ENV TZ=PST8PDT
ENTRYPOINT ["/server"]
