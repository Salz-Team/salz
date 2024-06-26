# Architecture Overview

![](build/overview.svg)

The architecture of our game can be split up into 3 collections of services.
There is the data collection consisting of object storage and a database.
There is the server which provides functionality to the end user via a frontend, an api, and a game visualizer.
And finally, there is a match runner which runs games between bots.

Here is a sketch of the functionality provided by each service:

- Data
  - Database
    - player: id, rank, botid
    - match: matchid, playerid, match-s3-location
  - Object Storage
    - match
    - bot
- Server
  - API
    - new player
    - upload bot
    - leaderboard
    - get match
    - match history
  - Frontend
    - new player
    - upload bot
    - leaderboard
    - match history
    - match visualizer
- Match Runner
  - Game Engine
    - game logic
    - local mode with visualizer
  - Match Maker
    - rank players
    - run matches
  - Match Handler
    - safely executes user code
    - coordinates user code and game engine

![](build/run_game.svg)

The above diagram gives an example of how we run matches.
First, the match maker reads the database to determine which pair of users should be in the next match.
Then the match handler pulls the code of each user, and safely executes it alongside the game engine.
After the game is done, the match handler saves the resulting gamefile to object storage and gives the ranker the result of the match.
The ranker will compute the new rankings of all of the players and update the database.

## API Endpoints:

### `/login`

Accessing the `/login` endpoint challenges the user's identity via Github. The callback for this is `/login/auth` which returns a JWT token.

The user visits `/login`, gets redirected to Github login, then redirected again to `/login/auth`

###  `/login/auth`

This endpoint is only for OAuth. After the user's identity is verified, an access token is sent back with this endpoint. We don't need to actually maintain access to the Github profile data. We encode the username, email, and JWT expiry time into a JWT token. The token is sent back in the following form:

```json
{
  "token": "eyj0eXAiOjk...."
}
```

Requests that require authorization via JWT must include an authorization header with the JWT token specifying the bearer schema. E.g.:

```
... headers
Authorization: Bearer ejy0eXAi0jk....
... more headers
```

### `/botfile`

Protected endpoint. Requires a signed JWT bearer token.

Handles the user's submission of a new bot.

### `/users`

Protected endpoint. Requires a signed JWT bearer token. 

Returns user data in the following form

```json
{
  "users": [
    {
      "elo" : 1200.2,
      "leaderboard" : 32,
      "login" : "feridun"
    }
    ....
  ]
}
```

where the email, login properties are the email and username of the Github account.

#### `/users/<login>/history`

Protected endpoint. Requires a signed JWT bearer token. 

Returns user game history data in the following form

```json
{
  "games": [
    {
      "gameid" : 0,
      "winner" : "feridun",
      "players" : [ "feridun", "feridob" ],
    }
    ....
  ]
}
```

where the email, login properties are the email and username of the Github account.

#### `/games/<gameid>`

Protected endpoint. Requires a signed JWT bearer token.

Returns a recording of a game.
```json
{
  "gameid" : 0,
  "winner" : "feridun",
  "players" : [ "feridun", "feridob" ],
  "serialized_game_log" : "????"
}
```

## Database Schema

We have users and we have games.

Users have an elo rank and a botfile.
The botfile is an s3 path to the users bot.

Games have an gamefile, users, a winner, a date, and a gamefile.
The gamefile is an s3 path to the game log.


```sql
CREATE TABLE IF NOT EXISTS users (userid SERIAL PRIMARY KEY,
                                  login VARCHAR,
                                  elo FLOAT,
                                  botfile VARCHAR,
                                  botstatus VARCHAR)
CREATE TABLE IF NOT EXISTS games (gameid SERIAL PRIMARY KEY,
                                  user1 INTEGER REFERENCES users(userid),
                                  user2 INTEGER REFERENCES users(userid),
                                  winner INTEGER REFERENCES users(userid),
                                  generated_at TIMESTAMP,
                                  gamefile VARCHAR)
```
