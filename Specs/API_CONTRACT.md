# API contract

Endpoints:

- `/frames/<framestart, frameend>`
  - default: last N frames (N = 50?)
  - `/frames?start=51&end=100`
  - `/frames?page=2`
  
  - `/frames/` return last N frames by default
  - `/frames?numFrames=123` sends last 123 frames
  - `/frames?dtstart=2019-04-03&dtend=2019-09-02` sends all frames between dtstart and dtend

- `/player/` -> 404 if not auth, else
  - tells all the shit in the players table

- POST, `/bots/upload` -> upload new bot and bot status
- GET, `/bots/status` -> prevbot, currbot dead, alive, building, etc

- `/signup/`

- `/logout/`

# examples


GET `/frames/`

```
{
  "frames": [
    {
      players: [
        {
          "playerid" : 13,
          "username" : "foobar123",
          "cells" : [
            { "x": 34, "y" : 32 },
            { "x": 54, "y" : 31 },
            { "x": 2, "y" : 9 }
          ],
        },
        {
          "playerid" : 14,
          "username" : "foobar321",
          "cells" : [
            { "x": 11, "y" : 11 },
            { "x": 12, "y" : 12 },
            { "x": 13, "y" : 13 }
          ],
        },
        ...
    }
    ...
  ],
  "dimensions" : {x : 1234, y : 1234}
}
```
