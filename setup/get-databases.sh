#!/bin/sh

aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/databases/northwind.sql.xz .
aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/databases/adventureworks.sql.xz .
