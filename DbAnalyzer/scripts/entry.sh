#!/bin/sh
echo "Starting DbAnalyzerSvc..."
exec /ktDbAnalyzerSvc/ktDbAnalyzerSvc-0.0.1/bin/ktDbAnalyzerSvc &
echo "Starting DbAnalyzer lambda..."
if [ -z "${AWS_LAMBDA_RUNTIME_API}" ]; then
  exec /usr/bin/aws-lambda-rie "$@"
else
  exec "$@"
fi
