CREATE SCHEMA IF NOT EXISTS salz;

CREATE TABLE IF NOT EXISTS salz.users (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    username TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    icon_path TEXT NOT NULL,
    identity_provider TEXT NOT NULL, -- only Github for now
    identity_provider_id TEXT NOT NULL, -- not all providers use numeric ids?
    elo FLOAT
);

-- upload_path is null when status is pending
-- status can be 'No status', 'Healthy', 'Unhealthy'
CREATE TABLE IF NOT EXISTS salz.bots (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    upload_path TEXT,
    user_id BIGINT NOT NULL,
    status TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES salz.users (id)
);
CREATE INDEX IF NOT EXISTS idx_bots_user_id ON salz.bots (user_id);
-- prevent name collisions in s3?
CREATE UNIQUE INDEX IF NOT EXISTS uq_bots_upload_path ON salz.bots (upload_path);

-- Status could have been an enumm. Allowable values:
--     'Pending', 'Running', 'Finished', 'Crashed'
CREATE TABLE IF NOT EXISTS salz.games (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    status TEXT NOT NULL, -- 'Pending', 'Running', 'Finished', 'Crashed'
    upload_path TEXT -- Where to upload game logs to
);

-- Score determins the winner of the game by comparing against
-- other participants. Is null until game finishes successfully.
CREATE TABLE IF NOT EXISTS salz.game_participants (
    game_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    bot_id BIGINT NOT NULL,
    score FLOAT,
    updated_at TIMESTAMP NOT NULL,
    FOREIGN KEY (game_id) REFERENCES salz.games (id),
    FOREIGN KEY (user_id) REFERENCES salz.users (id),
    FOREIGN KEY (bot_id) REFERENCES salz.bots (id),
    PRIMARY KEY (game_id, user_id)
);

CREATE SCHEMA auth;

-- Don't bother with FKs as we may move this to its
-- own database later.
CREATE TABLE IF NOT EXISTS auth.sessions (
    user_id BIGINT NOT NULL,
    token TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP NOT NULL
);

-- only one active session token per user.
CREATE UNIQUE INDEX IF NOT EXISTS idx_sessions_user_id_token ON auth.sessions (user_id);
-- Index to speed up session lookup by token
CREATE INDEX IF NOT EXISTS idx_sessions_token ON auth.sessions (token);
-- Index to speed up session cleanup
CREATE INDEX IF NOT EXISTS idx_sessions_expires_at ON auth.sessions (expires_at);
