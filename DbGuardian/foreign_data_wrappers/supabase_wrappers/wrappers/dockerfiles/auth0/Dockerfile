FROM python:3.13.0a4-slim

WORKDIR /usr/src/app

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl

COPY ./server.py .

CMD [ "python", "./server.py" ]
