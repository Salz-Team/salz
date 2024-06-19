#!/usr/bin/env bash

SALZ_SECRET_LOCATION="../../salz-secrets"
SALZ_SECRETS=$($SALZ_SECRET_LOCATION/decrypt.sh $SALZ_SECRET_LOCATION/dev-secrets.json.age)

export OAUTH_CLIENT_KEY=$(echo $SALZ_SECRETS | jq -r '.oauth_client_id')
export OAUTH_CLIENT_SECRET=$(echo $SALZ_SECRETS | jq -r '.oauth_client_secret')

export ENV="development"
 
go run .
