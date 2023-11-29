FROM alpine:3.18.4
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Web Front End"
RUN apk update && apk add ca-certificates && apk add libpq && apk add gmp && apk add gcompat && rm -rf /var/cache/apk/*      
RUN apk add curl
RUN apk add --update --no-cache openssl
COPY server /server
COPY admin /admin
COPY customer /customer
COPY mallard /mallard
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/server"]
