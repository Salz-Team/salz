INSERT INTO salz.users (username, icon_path, identity_provider, identity_provider_id) VALUES
('testuser1', 'user1.png', 'fakeprovider', 'fakeid1'),
('testuser2', 'user2.png', 'fakeprovider', 'fakeid2'),
('testuser3', 'user3.png', 'fakeprovider', 'fakeid3'),
('testuser4', 'user4.png', 'fakeprovider', 'fakeid4'),
('testuser5', 'user5.png', 'fakeprovider', 'fakeid5'),
('testuser6', 'user6.png', 'fakeprovider', 'fakeid6'),
('testuser7', 'user7.png', 'fakeprovider', 'fakeid7'),
('testuser8', 'user8.png', 'fakeprovider', 'fakeid8'),
('testuser9', 'user9.png', 'fakeprovider', 'fakeid9'),
('testuser10', 'user10.png', 'fakeprovider', 'fakeid10');

-- Create a bot for each user
INSERT INTO salz.bots (user_id, upload_path, status)
SELECT
    id,
    'bots/' || id || '.zip' AS upload_path,
    'healthy' AS status
FROM salz.users;

-- Create a bunch of games for each user
INSERT INTO salz.games (status)
SELECT 'finished' FROM generate_series(1, 50);

-- Create a bunch of game results for each game via game participants
INSERT INTO salz.game_participants (game_id, user_id, bot_id, score, updated_at)
WITH game_users AS (
    SELECT DISTINCT ON (g.id, u.id)
        g.id AS game_id,
        u.id AS user_id,
        b.id AS bot_id,
        random() AS score
    FROM salz.games g
    JOIN salz.bots b
        ON b.user_id = u.id
    CROSS JOIN salz.users AS u
    ORDER BY g.id ASC, u.id ASC, b.created_at DESC
),

game_users_random_order AS (
    SELECT
        game_id,
        user_id,
        bot_id,
        row_number() OVER (PARTITION BY game_id ORDER BY random()) AS rand_order,
        score
    FROM game_users
)

SELECT
    game_id,
    user_id,
    bot_id,
    score,
    now() AS updated_at
FROM game_users_random_order
WHERE rand_order <= 5;
