FROM python:3.13.0a6-slim

WORKDIR /usr/src/app

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl \
    && pip install airtablemock

COPY ./server.py .

CMD [ "python", "./server.py" ]
