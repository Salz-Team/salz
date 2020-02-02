# Salz

**Salz** is pronounced like 'salt', drawing from the fact that the game
is very strongly inspired by [Halite](https://halite.io/),
which also based its name on salt.
Salz is essentially Conway's Game of Life, 
made for a multiplayer universe,
where everyone build their own bots to play the game.



## Game Rules

Salz is a game based on Conway's Game of Life.
In Salz the world consists of a large grid (it wraps into a torus) of cells.
Each cell is either dead or is alive in which case it belongs to a player.

Each player begins with one live cell and every turn can create 3 new cells.
After all players new cells are created the Game of life rules calculate the
next state of the world.
The goal is simply to stay alive as you come into contact with other players
and to see what you can create in the open world.

A player may only flip up to 3 cells every turn.
You may only flip a cell if you own it or if it is
adjacent to a cell that you own.
Further more you may only flip a cell if within 30 cells there is no cell
belonging to another player.

So in order to maintain control you must make sure that you have some area
of 30 cells that are clear of other players.

If two players cells are next to each other the game of life rules continue
as normal. The only extension is that when a new cell is created, it belongs
to the player who has the most cells around it (if the number of players
are tied then the order should be random, which is not yet implemented).

