CREATE SCHEMA IF NOT EXISTS spicerack;

CREATE TABLE IF NOT EXISTS spicerack.users (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    username TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    icon_path TEXT,
    identity_provider TEXT NOT NULL, -- only Github for now
    identity_provider_id TEXT NOT NULL, -- not all providers use numeric ids?
    elo FLOAT
);

INSERT INTO spicerack.users (username, identity_provider, identity_provider_id) 
VALUES ('admin@salz.life', 'noauth', 'admin@salz.life')
ON CONFLICT DO NOTHING;


CREATE UNIQUE INDEX IF NOT EXISTS uq_users_username ON spicerack.users (username);

CREATE TABLE IF NOT EXISTS spicerack.games (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    created_by BIGINT NOT NULL,
    icon_path TEXT,
    FOREIGN KEY (created_by) REFERENCES spicerack.users (id)
);

INSERT INTO spicerack.games (name, created_by)
VALUES ('salz', (select id from spicerack.users where username = 'admin@salz.life' limit 1))
ON CONFLICT DO NOTHING;

-- upload_path is null when status is pending
-- status can be 'No status', 'Healthy', 'Unhealthy'
CREATE TABLE IF NOT EXISTS spicerack.bots (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    game_id BIGINT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    upload_path TEXT,
    user_id BIGINT NOT NULL,
    status TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES spicerack.users (id),
    FOREIGN KEY (game_id) REFERENCES spicerack.games (id)
);
CREATE INDEX IF NOT EXISTS idx_bots_gameid_userid ON spicerack.bots (game_id, user_id);
-- prevent name collisions in s3?
CREATE UNIQUE INDEX IF NOT EXISTS uq_bots_upload_path ON spicerack.bots (upload_path);

CREATE TABLE IF NOT EXISTS spicerack.matches (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    game_id BIGINT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    status TEXT NOT NULL, -- 'Pending', 'Running', 'Finished', 'Crashed'
    upload_path TEXT, -- Where to upload match logs to (S3 key)
    FOREIGN KEY (game_id) REFERENCES spicerack.games (id),
    CHECK (status in ('Pending', 'Running', 'Finished', 'Crashed'))
);

-- Score determins the winner of the match by comparing against
-- other participants. Is null until match finishes successfully.
CREATE TABLE IF NOT EXISTS spicerack.match_participants (
    game_id BIGINT NOT NULL,
    match_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    bot_id BIGINT NOT NULL,
    score FLOAT,
    updated_at TIMESTAMP NOT NULL,
    FOREIGN KEY (game_id) REFERENCES spicerack.games (id),
    FOREIGN KEY (match_id) REFERENCES spicerack.matches (id),
    FOREIGN KEY (user_id) REFERENCES spicerack.users (id),
    FOREIGN KEY (bot_id) REFERENCES spicerack.bots (id),
    PRIMARY KEY (game_id, match_id, user_id)
);

CREATE SCHEMA auth;

-- Don't bother with FKs as we may move this to its
-- own database later.
CREATE TABLE IF NOT EXISTS auth.sessions (
    user_id BIGINT NOT NULL,
    token TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT (NOW() AT TIME ZONE 'utc'),
    expires_at TIMESTAMP NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_sessions_user_id_token ON auth.sessions (user_id); -- only one active session token per user.
CREATE INDEX IF NOT EXISTS idx_sessions_token ON auth.sessions (token); -- Index to speed up session lookup by token
CREATE INDEX IF NOT EXISTS idx_sessions_expires_at ON auth.sessions (expires_at); -- Index to speed up session cleanup

-- Tables used for basic auth

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS auth.basic_auth_logins (
    user_id BIGINT NOT NULL PRIMARY KEY,
    pw_hash TEXT NOT NULL
);
