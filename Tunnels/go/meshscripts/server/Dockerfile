FROM alpine:3.18.5
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Mesh Server"
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*      
RUN apk add curl
COPY meshserver /meshserver
EXPOSE 30000-30050
EXPOSE 80
EXPOSE 3009
ENV TZ=PST8PDT
ENTRYPOINT ["/meshserver"]
