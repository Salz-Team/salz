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
SELECT id, 'bots/' || id || '.zip', 'healthy' FROM salz.users;

-- Create a bunch of games for each user
INSERT INTO salz.games (status)
SELECT 'finished' FROM generate_series(1, 50);

-- Create a bunch of game results for each game via game participants
INSERT INTO salz.game_participants (game_id, user_id, bot_id, score, updated_at)
with game_users as (
    select distinct on (g.id, u.id)
        g.id as game_id,
        u.id as user_id,
        b.id as bot_id,
        random() as score
    from salz.games g
    cross join salz.users u
    join salz.bots b
        on b.user_id = u.id
    order by g.id, u.id, b.created_at desc
), game_users_random_order as (
    select
        game_id,
        user_id,
        bot_id,
        row_number() over (partition by game_id order by random()) as rand_order,
        score
    from game_users
)
select
    game_id,
    user_id,
    bot_id,
    score,
    now() as updated_at
from game_users_random_order
where rand_order <= 5
;



