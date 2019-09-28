CREATE TABLE IF NOT EXISTS players (playerid SERIAL PRIMARY KEY, 
                                    username VARCHAR,
                                    botdir VARCHAR,
                                    updatedbot BOOL,
                                    newbotdir VARCHAR,
                                    botstatus VARCHAR)
