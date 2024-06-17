CREATE TABLE IF NOT EXISTS users (userid SERIAL PRIMARY KEY,
                                  login VARCHAR,
                                  elo FLOAT,
                                  botfile VARCHAR,
                                  botstatus VARCHAR);

CREATE TABLE IF NOT EXISTS games (gameid SERIAL PRIMARY KEY,
                                  user1 INTEGER REFERENCES users(userid),
                                  user2 INTEGER REFERENCES users(userid),
                                  winner INTEGER REFERENCES users(userid),
                                  generated_at TIMESTAMP,
                                  gamefile VARCHAR)
