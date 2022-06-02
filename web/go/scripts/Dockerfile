FROM alpine:latest
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Web Front End"
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*      
COPY server /server
COPY admin /admin
COPY customer /customer
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/server"]
