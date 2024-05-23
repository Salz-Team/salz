# Architecture Overview

![](../build/diagram.svg)

Here is a sketch of the functionality provided by each service:

- Database
  - player: id, rank, botid
  - match: matchid, playerid, match-s3-location
- Object Storage
  - match
  - bot
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
- Game Engine
  - game logic
  - local mode with visualizer
- Match Maker
  - rank players
  - run matches
- Bot Handler
  - execute user code safely
