#!/usr/bin/env bash

set -Eeuo pipefail

# Check that the `hcp` command is available
command -v hcp >/dev/null || { echo >&2 "The hcp command is required but it's not installed. Aborting."; exit 1; }
# Check that we're logged in
hcp auth print-access-token >/dev/null 2>&1 || { echo >&2 "Not logged into HCP -- login via hcp auth login"; exit 1; }

export OAUTH_CLIENT_KEY=$(hcp vault-secrets secrets open github_oauth_client_id --format json | jq -r '.static_version.value')
export OAUTH_CLIENT_SECRET=$(hcp vault-secrets secrets open github_oauth_client_secret --format json | jq -r '.static_version.value')

go run .
