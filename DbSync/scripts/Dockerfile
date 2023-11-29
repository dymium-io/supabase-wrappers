FROM public.ecr.aws/lambda/provided:al2

RUN yum update -y
COPY main entry.sh /
ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN chmod 755 /usr/bin/aws-lambda-rie /entry.sh /main
ENTRYPOINT ["/entry.sh"]
