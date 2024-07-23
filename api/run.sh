#!/usr/bin/env bash

# Check that the `hcp` command is available
command -v hcp >/dev/null 2>&1 || { echo >&2 "The hcp command is required but it's not installed. Aborting."; exit 1; }

export ENV="local"

export OAUTH_CLIENT_KEY=$(hcp vault-secrets secrets open github_oauth_client_id --format json | jq -r '.static_version.value')
export OAUTH_CLIENT_SECRET=$(hcp vault-secrets secrets open github_oauth_client_secret --format json | jq -r '.static_version.value')

# Local environment variables
export PG_URI="postgres://salz:superdupersecret@localhost:5432/salz?sslmode=disable"
export MINIO_ENDPOINT="localhost:9110"
export MINIO_ACCESS_KEY="minioadmin"
export MINIO_SECRET_KEY="minioadmin"
export MINIO_USE_SSL="false"
 
go run .
