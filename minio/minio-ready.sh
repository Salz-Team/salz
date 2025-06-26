#!/usr/bin/env bash

if ! mc ls local/salz | grep "salz"
then
    echo "Salz bucket doesn't exist in local minio, creating"
    mc mb local/salz
fi
