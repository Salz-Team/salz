FROM postgres:latest

COPY ./docker-entrypoint-initdb.d /docker-entrypoint-initdb.d
## mount the folder

ENV POSTGRES_PASSWORD mysecretpassword
ENV POSTGRES_USER postgres
ENV POSTGRES_DB postgres


EXPOSE 5432
