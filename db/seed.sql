INSERT INTO spicerack.users (username, created_at, updated_at, icon_path, identity_provider, identity_provider_id, elo)
WITH created_at AS (
    SELECT
        'testuser' || g AS username,
        now() - (random() * interval '90 days') AS created_at,
        's3://users/profile_pictures/user' || g || '.png' AS icon_path,
        'basicauth' AS identity_provider,
        'testuser' || g AS identity_provider_id,
        round(1000 + (random() * 1000)) AS elo -- between 1000 and 2000, uniform
    FROM generate_series(1, 100) g
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

-- Create a "game" called salz
INSERT INTO spicerack.games (name, created_by) VALUES ('Salz', (select id from spicerack.users order by id limit 1));

-- Create a bot for each user
INSERT INTO spicerack.bots (user_id, game_id, upload_path, status, created_at, updated_at)
WITH created AS (
    SELECT
        id,
        (select id from spicerack.games where name = 'Salz') as game_id,
        -- s3://games/<game_id>/bots/<user_id>/<bot_id>.zip
        's3://games/' || (select id::text from spicerack.games where name = 'Salz') || '/bots/' || id || '/' || gen_random_uuid() || '.zip' AS upload_path,
        'healthy' AS status,
        updated_at + (random() * interval '14 days') AS created_at
    FROM spicerack.users
)

SELECT
    id AS user_id,
    game_id,
    upload_path,
    status,
    created_at,
    created_at + interval '15 minutes' AS updated_at -- time it takes to upload and validate healthy?
FROM created;

-- Create a bunch of games for each user
-- Start dates are random between 1 and 90 days ago
-- End dates are random between 1 and 10 days after start date
INSERT INTO spicerack.matches (game_id, created_at, updated_at, status)
WITH created_status AS (
    SELECT
        now() - (random() * (interval '90 days')) + '30 days' AS created_at,
        (ARRAY['Pending', 'Running', 'Finished', 'Crashed'])[floor(random() * 4 + 1)] AS status
    FROM generate_series(1, 50)
)

SELECT -- fill in the end date depending on the status
    (select id from spicerack.games where name = 'Salz') as game_id,
    created_at,
    CASE
        WHEN status = 'Pending' THEN created_at
        WHEN status = 'Running' THEN created_at + (random() * interval '10 minutes') -- takes between 0 - 10 minutes to go from pending to running
        WHEN status = 'Finished' THEN created_at + (random() * interval '10 minutes') + (random() * interval '1 hour') -- takes 0 - 60 minutes to go from running to finished
        WHEN status = 'Crashed' THEN created_at + (random() * interval '10 minutes') + (random() * interval '15 minutes') -- takes 0 - 15 minutes to go from running to crashed
    END AS updated_at,
    status
FROM created_status;

-- Fill in the upload_path for finished or crashed games
UPDATE spicerack.matches
SET upload_path = 's3://games/' || game_id || '/match_history/' || id || '/' || id || '-logs.jsonl'
WHERE status in ('Finished', 'Crashed');

-- Create a bunch of game results for each game via game participants
-- for each game, get the most recent bots before the creation date of the game
-- and sample 5 of them to be the participants in the game
-- the score is random
INSERT INTO spicerack.match_participants (game_id, match_id, user_id, bot_id, score, updated_at)
WITH match_all_valid_bots AS (
    SELECT DISTINCT ON (m.id, b.user_id)
        m.id AS match_id,
        m.created_at AS match_created_at,
        m.updated_at AS match_updated_at,
        m.status AS match_status,
        b.id AS bot_id,
        b.user_id
    FROM spicerack.matches m
    CROSS JOIN spicerack.bots b
    WHERE b.created_at < m.created_at
    ORDER BY m.id ASC, b.user_id ASC, b.created_at DESC -- distinct by (game, user_id) and most recent bot
),

match_random_participants AS (
    SELECT
        match_id,
        match_created_at,
        match_updated_at,
        match_status,
        bot_id,
        user_id,
        row_number() OVER (PARTITION BY match_id ORDER BY random()) AS rand_order
    FROM match_all_valid_bots
)

SELECT
    (select id from spicerack.games where name = 'Salz') as game_id,
    match_id,
    user_id,
    bot_id,
    CASE
        WHEN match_status = 'Finished' THEN random()
    END AS score,
    match_updated_at AS updated_at
FROM match_random_participants WHERE rand_order <= 5;
