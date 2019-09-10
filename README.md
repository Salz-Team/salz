# Oba
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
