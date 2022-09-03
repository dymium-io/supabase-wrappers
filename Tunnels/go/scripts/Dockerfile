FROM alpine:latest
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Tunneling Server"
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*      
COPY server /server
EXPOSE 46791
ENV TZ=PST8PDT
ENTRYPOINT ["/server"]
