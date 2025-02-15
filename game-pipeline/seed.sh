#!/usr/bin/env bash

ppsql -c "
INSERT INTO salz.users (username, created_at, updated_at, icon_path, identity_provider, identity_provider_id, elo)
WITH created_at AS (
    SELECT
        'testuser' || g AS username,
        now() - (random() * interval '90 days') AS created_at,
        'user' || g || '.png' AS icon_path,
        'basicauth' AS identity_provider,
        'testuser' || g AS identity_provider_id,
        round(1000 + (random() * 1000)) AS elo -- between 1000 and 2000, uniform
    FROM generate_series(1, 10) g
)

SELECT
    username,
    created_at,
    created_at + interval '1 day' AS updated_at,
    icon_path,
    identity_provider,
    identity_provider_id,
    elo
FROM created_at;

-- Create a bot for each user
INSERT INTO salz.bots (user_id, upload_path, status, created_at, updated_at)
WITH created AS (
    SELECT
        id,
        'bots/' || id || '.zip' AS upload_path,
        'healthy' AS status,
        updated_at + (random() * interval '14 days') AS created_at
    FROM salz.users
)

SELECT
    id AS user_id,
    upload_path,
    status,
    created_at,
    created_at + interval '15 minutes' AS updated_at -- time it takes to upload and validate healthy?
FROM created;
"



# Seed Bots for our Players
echo "cabal run tic-tac-toe-player" > run.sh
zip bot.zip run.sh
for i in {0..10}; do
  mc cp bot.zip local/salz/bots/${i}.zip
done
rm run.sh
