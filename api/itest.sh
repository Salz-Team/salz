#!/usr/bin/env bash

cleanup_container() {
    local CONTAINER_NAME=$1
    podman stop -i "$CONTAINER_NAME" &> /dev/null || true
    podman rm -i "$CONTAINER_NAME" &> /dev/null || true
}

retry_delay() {
    local retries=5
    local delay=2
    local count=0

    while [ $count -lt $retries ]; do
        "$@" && return 0
        count=$((count + 1))
        sleep $delay
    done

    echo "Command failed after $retries attempts."
    return 1
}

random_free_port() {
    comm -23 <(seq 1024 65535 | sort) <(ss -tuln | awk 'NR>1 {print $4}' | awk -F: '{print $NF}' | sort -u) | shuf -n1
}

TEST_PREFIX="salz-api-itest"
MINIO_BUCKET="spicerack"
SALZ_BUCKET=$MINIO_BUCKET
POSTGRES_PORT="$(random_free_port)"
MINIO_PORT="$(random_free_port)"

PG_USER=postgres
PG_DB=postgres
PG_PASS=mysecretpassword

cleanup_container $TEST_PREFIX-postgres
cleanup_container $TEST_PREFIX-minio

# Start postgres
echo "Starting postgres, reserved port $POSTGRES_PORT..."
podman run -d \
    -e "POSTGRES_USER=${PG_USER}" \
    -e "POSTGRES_DB=${PG_DB}" \
    -e "POSTGRES_PASSWORD=${PG_PASS}" \
    -p ${POSTGRES_PORT}:5432 \
    --name "${TEST_PREFIX}-postgres" \
    --healthcheck-command="PGPASSWORD=${PG_PASS} psql --username=${PG_USER} --dbname=${PG_DB} -c 'select 1'" \
    docker.io/postgres:16

# Start MinIO
echo "Starting MinIO, reserved port $MINIO_PORT..."
podman run -d \
    --name "${TEST_PREFIX}-minio" \
    -p ${MINIO_PORT}:9000 \
    --healthcheck-command="MC_HOST_local=http://minioadmin:minioadmin@localhost:9000 mc ping local --count 1" \
    docker.io/minio/minio:latest server /data

# Check that postgres and MinIO are available
echo "Healthcheck: postgres ..."
retry_delay podman healthcheck run ${TEST_PREFIX}-postgres &> /dev/null
if [ $? -ne 0 ]; then
    echo "Postgres healthcheck failed"
    cleanup
    exit 1
fi

echo "Healthcheck: MinIO ..."
retry_delay podman healthcheck run ${TEST_PREFIX}-minio &> /dev/null
if [ $? -ne 0 ]; then
    echo "MinIO healthcheck failed"
    cleanup
    exit 1
fi

echo "Both healthchecks passed!"

# Initialize the database
cat ../db/init.sql \
    | podman exec -i ${TEST_PREFIX}-postgres /bin/bash -c "PGPASSWORD=${PG_PASS} psql --username=${PG_USER} --dbname=${PG_DB} -f /dev/stdin "

# Initialize the S3 bucket
podman exec -i ${TEST_PREFIX}-minio /bin/bash -c "MC_HOST_local=http://minioadmin:minioadmin@localhost:9000 mc mb local/${MINIO_BUCKET}"

# Run the integration tests
command -v hcp >/dev/null 2>&1 || { echo >&2 "The hcp command is required but it's not installed. Aborting."; exit 1; }

export ENV="itest"

export PG_URI="postgres://${PG_USER}:${PG_PASS}@localhost:${POSTGRES_PORT}/${PG_DB}?sslmode=disable"
export WEB_BASEURL="http://localhost:5173"
export MINIO_ENDPOINT="localhost:${MINIO_PORT}"
export MINIO_ACCESS_KEY="minioadmin"
export MINIO_SECRET_KEY="minioadmin"
export MINIO_USE_SSL="false"
export SALZ_BUCKET="spicerack"

ENV="itest" go test -v --tags=integration ./test/...
