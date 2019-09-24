FROM postgres:latest

COPY ./docker-entrypoint-initdb.d /docker-entrypoint-initdb.d
## mount the folder

ENV POSTGRES_DB postgres
ENV POSTGRES_USER postgres
ENV POSTGRES_PASSWORD mysecretpassword

EXPOSE 5432
