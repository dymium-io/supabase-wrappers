FROM alpine:3.18.4
LABEL com.dymium.vendor="Dymium" \
      version="0.1" \
      product="Dymium Machine To Machine Tunnel" 
RUN apk update && apk add ca-certificates && rm -rf /var/cache/apk/*
COPY machineclient /machineclient
EXPOSE 80
ENV TZ=PST8PDT
ENTRYPOINT ["/machineclient"]
