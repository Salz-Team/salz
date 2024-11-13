INSERT INTO salz.users (username, created_at, updated_at, icon_path, identity_provider, identity_provider_id, elo)
with created_at as (
    select
        'testuser' || g as username,
        now() - (random() * interval '90 days') as created_at,
        'user' || g || '.png' as icon_path,
        'basicauth' as identity_provider,
        'testuser' || g as identity_provider_id,
        round(1000 + (random() * 1000)) as elo -- between 1000 and 2000, uniform
    from generate_series(1, 100) g
)
select
    username,
    created_at,
    created_at + interval '1 day' as updated_at,
    icon_path,
    identity_provider,
    identity_provider_id,
    elo
from created_at;

-- Create a bot for each user
INSERT INTO salz.bots (user_id, upload_path, status, created_at, updated_at)
with created as (
    SELECT
        id,
        'bots/' || id || '.zip' AS upload_path,
        'healthy' AS status,
        updated_at + (random() * interval '14 days') as created_at
    FROM salz.users
)
select
    id as user_id,
    upload_path,
    status,
    created_at,
    created_at + interval '15 minutes' as updated_at -- time it takes to upload and validate healthy?
from created;

-- Create a bunch of games for each user
-- Start dates are random between 1 and 90 days ago
-- End dates are random between 1 and 10 days after start date
INSERT INTO salz.games (created_at, updated_at, status)
with created_status as (
    SELECT
        NOW() - (random() * (interval '90 days')) + '30 days' as created_at,
        (array['Pending', 'Running', 'Finished', 'Crashed'])[floor(random() * 4 + 1)] as status
    FROM generate_series(1, 50)
)
select -- fill in the end date depending on the status
    created_at,
    case
        when status = 'Pending' then created_at
        when status = 'Running' then created_at + (random() * interval '10 minutes') -- takes between 0 - 10 minutes to go from pending to running
        when status = 'Finished' then created_at + (random() * interval '10 minutes') + (random() * interval '1 hour') -- takes 0 - 60 minutes to go from running to finished
        when status = 'Crashed' then created_at + (random() * interval '10 minutes') + (random() * interval '15 minutes') -- takes 0 - 15 minutes to go from running to crashed
    end as updated_at,
    status
from created_status;

-- Create a bunch of game results for each game via game participants
-- for each game, get the most recent bots before the creation date of the game
-- and sample 5 of them to be the participants in the game
-- the score is random
INSERT INTO salz.game_participants (game_id, user_id, bot_id, score, updated_at)
with game_all_valid_bots as (
    select distinct on (g.id, b.user_id)
        g.id as game_id,
        g.created_at as game_created_at,
        g.updated_at as game_updated_at,
        g.status as game_status,
        b.id as bot_id,
        b.user_id as user_id
    from salz.games g
    cross join salz.bots b
    where b.created_at < g.created_at
    order by g.id, b.user_id, b.created_at desc -- distinct by (game, user_id) and most recent bot
), game_random_participants as (
    select
        game_id,
        game_created_at,
        game_updated_at,
        game_status,
        bot_id,
        user_id,
        row_number() over (partition by game_id order by random()) as rand_order
    from game_all_valid_bots
)
select
    game_id,
    user_id,
    bot_id,
    case
        when game_status = 'Finished' then random()
        else null
    end as score,
    game_updated_at as updated_at
from game_random_participants where rand_order <= 5;
