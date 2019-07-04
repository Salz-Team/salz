# Overview

- game.hs :: plays 2 players
- matchmaker.hs :: finds potential matchups to minimize delta
- tournament.hs :: orchestrates matchmaker with database, ranker, and game.hs to run matches
- apiserver.hs :: servant that adds players to database/access match history...

The database should have:
- games history :: all games ever played

- players :: all bots ever submitted, all games ever played

# Documentation

## How to Run a Game

To run a game just give the `game` program the width and height of the map and then give it instructions on how to run each player. For example:

```sh
./game 30 30 'python starter\ kits/player.py' 'python starter\ kits/player.py'
//game width height player1 player2
```

## Game Output Format

The format is:
```
mapwidth mapheight
player1name player2name
```
followed by coordinate pairs for every cell turned on.

For example:
```
100 100
Player1 Player2
1 2 1 4 1 2 3 1 
1 2 1 4 1 2 3 1 
1 2 1 4 1 2 3 1 
1 2 1 4 1 2 3 1 
1 2 1 4 1 2 3 1 
```

## Player Input/Output Format
Each player may assume that they are on the left side


## Todo
Player owned cells,  
