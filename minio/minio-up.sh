#!/usr/bin/env bash
if ! test -d "$STATEDIR"
then
    echo "$STATEDIR folder not found -- are you running this from the repository root?"
    exit 1
fi

minio server --json "$STATEDIR/minio"
