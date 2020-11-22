#!/bin/sh
export PGNAME=postgres
export PGUSER=postgres
export PGPASS=postgres
export PGHOST=localhost
export PGPORT=5432

go run main.go k8s.go
