# Salz

## Database and API setup (non-dev)

If you need the database and API to work, use the provided `docker-compose.yml` file to spin up these services.

`salz-db` will run on `localhost:5432`

`salz-api` will run on `localhost:8080`

Make sure you have `docker` and `docker-compose` installed. To make sure `docker` is installed and running correctly (i.e., you have added yourself to the `Docker` group, and other weird linux stuff you need to get docker going) run `docker run hello-world`. Install `docker-compose`.

In the root directory, you can simply run 

```
docker-compose up
```

This will search for a `docker-compose.yml` file (which is in the root directory) and build the necessary images. Additionally, the logs from both services are tailed.

If you happen to make any changes to the database or API, you have to rebuild the images. The next time you need the services, run `docker-compose` with the `--build` flag. I.e.,

```
docker-compose up --build
```


For now, hit the `/frames/` endpoint. I.e., `localhost:8080/frames`

## Database
I am going to use sqlite.
Todo:
- setup database
- setup types and code to store
  - players
    - playerid
    - botlocation :: string
  - history
    - map
## Directory Breakup
### Board
Responsible for board geometry
### External Process Handler
Responsible for handling an external process
**Interface**:
- createExternalProcess
- timedCallandResponse
- replaceExecutable
### PlayerBot Handler
Responsible for the PlayerBot Interface
Todo:
- create status for the player bot
### Player
Responsible for player game information:
- what a player can see
- what a player can effect
- the starting location of a player
**Interface**:
### Game
Responsible for game logic:
- gameStep :: going from one state to the next
- gameLoop :: tying together gameStep with player and playerIOHandler
# Todo
- [ ] Save the game to json and to database
- [ ] Create the resource to be mined (to allow for more power)
- [ ] Allow creation of new players
