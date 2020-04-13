CREATE TABLE IF NOT EXISTS players (playerid SERIAL PRIMARY KEY, 
                                    username VARCHAR,
                                    botdir VARCHAR,
                                    botmemory VARCHAR,
                                    botstderr VARCHAR,
                                    errormsg VARCHAR
                                    )
