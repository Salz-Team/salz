CREATE SCHEMA IF NOT EXISTS salz;

CREATE TABLE IF NOT EXISTS salz.users (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    username TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    icon_path TEXT NOT NULL,
    identity_provider TEXT NOT NULL, -- could be an enum: 'Github', 'Google', 'Facebook', etc.
    identity_provider_id TEXT NOT NULL, -- Github internal IDs are numeric, but maybe not other providers?,
    elo FLOAT
);
CREATE UNIQUE INDEX IF NOT EXISTS uq_users_username ON salz.users (username);

CREATE TABLE IF NOT EXISTS salz.bots (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    upload_path TEXT, -- Null when status is pending
    user_id BIGINT NOT NULL,
    status TEXT NOT NULL, -- could be an enum: 'No Status', 'Healthy', 'Unhealthy'
    FOREIGN KEY (user_id) REFERENCES salz.users (id)
);
CREATE INDEX IF NOT EXISTS idx_bots_user_id ON salz.bots (user_id);
CREATE UNIQUE INDEX IF NOT EXISTS uq_bots_upload_path ON salz.bots (upload_path); -- Prevent name collisions in s3?

CREATE TABLE IF NOT EXISTS salz.games (
    id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    status TEXT NOT NULL, -- could be an enum: 'Pending', 'Running', 'Finished', 'Crashed'
    upload_path TEXT -- Where to upload game logs to
);

CREATE TABLE IF NOT EXISTS salz.game_participants (
    game_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    bot_id BIGINT NOT NULL,
    score FLOAT, -- Winner of game determined by score amongst other participants. Null until game finishes successfully.
    updated_at TIMESTAMP NOT NULL,
    FOREIGN KEY (game_id) REFERENCES salz.games (id),
    FOREIGN KEY (user_id) REFERENCES salz.users (id),
    FOREIGN KEY (bot_id) REFERENCES salz.bots (id),
    PRIMARY KEY (game_id, user_id)
);

CREATE SCHEMA auth;

-- May consider moving this to its own database later, so let's not bother with FKs for now.
CREATE TABLE IF NOT EXISTS auth.sessions (
    user_id BIGINT NOT NULL,
    token TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
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
