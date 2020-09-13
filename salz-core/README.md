# The Game Engine
## Running the Game Engine

The game engine can run in two modes: 
1. server mode: the engine writes to a postgresql database and reads playerinfo
     through the same database.
2. local mode: the engine writes to a file(sqlite database) and reads 
     playerinfo in through the command line.

## Server Mode
The game engine and database is wrapped in a docker container that you can setup
and run using `docker build .` followed by `docker-compose up`.

Note that getting the docker container up and running takes a long time and right
now it is not properly cached so it needs to rebuild most of itself on every
change.

For development we recommend using the local mode or if you need to test the 
postgresql library, run it using stack. To do this you need to:
1. run the database: run the docker file in `../salz-db`
2. build the project: `stack build`
3. run the game-engine: `stack ghci` then run the command in `./app/Main.hs` 
