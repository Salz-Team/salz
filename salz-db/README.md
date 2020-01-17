# Salz Database

## Database Schema

The player table contains:
```
playerid SERIAL, username VARCHAR, updatedbot VARCHAR, botstatus VARCHAR
```

~updatedbot~

When a new bot is uploaded the api should put the new bot's location into the `updatedbot`
field. The botbuilder will check to see if this field is empty or not at the start of every
turn. If it is not empty the botbuilder will attempt to build whatever the field points to.

~botstatus~

The botstatus will contain the message "All good" if the bot is running as it should, and
a relevant error message otherwise. This field will be updated by the game engine and bot
builder.

The snapshot table contains:
```
id SERIAL, turnId INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP
```

This table contains a snapshot of the map at the end of certain turns.

The snapshot table works with the moves table to give you information on the game.

The moves table contains:
```
id SERIAL, turnId INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP
```

While the schema is identical to the snapshots table, this table doesn't hold snapshots
of the map, instead every move that a player makes is recorded. 

Since Conway's Game of Life is deterministic you can combine the latest snapshot of
the map and all of the moves since said snapshot to calculate the full state of the 
game since then.
