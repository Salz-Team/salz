CREATE TABLE IF NOT EXISTS moves (id SERIAL PRIMARY KEY, 
                   turnId INTEGER,
                   x INTEGER,
                   y INTEGER,
                   playerid INTEGER REFERENCES players(playerid),
                   generated_at TIMESTAMP)
