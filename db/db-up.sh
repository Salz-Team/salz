#!/usr/bin/env bash
if ! test -d ".state"
then
    echo ".state folder not found -- are you running this from the repository root?"
    exit 1
fi

echo "PGDATABASE $PGDATABASE"
echo "PGDATA $PGDATA"
echo "PGHOST $PGHOST"

trap 'pg_ctl -D $PGDATA stop' EXIT

if ! test -d "$PGDATA"
then
    echo "PGDATA directory $PGDATA does not exist -- running setup"
    initdb -U salz -A md5 --pwfile=<(echo superdupersecret) "$PGDATA"
    OPT="unix_socket_directories"
    sed -i "s|^#$OPT.*$|$OPT = '$PGDATA'|" "$PGDATA/postgresql.conf"
    pg_ctl -D "$PGDATA" -l "$PGDATA/postgres.log"  start
    PGPASSWORD=superdupersecret createdb -U salz
    PGPASSWORD=superdupersecret psql -U salz -f "$PWD/db/init.sql"
    pg_ctl -D "$PGDATA" stop
fi

pg_ctl -D "$PGDATA" -l "$PGDATA/postgres.log"  start
tail -f "$PGDATA/postgres.log"

