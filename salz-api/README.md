# salz-api

## Endpoints

Endpoints currently implemented

### `/frames/`

`/frames` returns JSON of the following form:

```
{
	"frames": [
		[
			{
				"turnid" : 3
				"playerid" : 5
				"pos" : [
					{"x" : 34, "y" : 12}
					...
				]
			}
			...
		]
		...
	]
}
```

i.e., the `frames` value gives a list of list of playercell objects. E.g.: `frames[0]` gives the first frame. `frames[0][2]` gives the third playercell object of that frame (0-indexed). A playercell object has the form

```
{
	"turnid" : 123
	"playerid" : 321
	"pos" : [
		{"x" : 1111, "y" : 234}
		...
	]
}
```

where `pos` lists all cells on the map owned by player `playerid` during frame `turnid`.

#### Query strings

Order of precedence is the order of this listing. E.g., if you pass `numframes` as well as `startframe` and `endframe`, the start/end frame data is evaluated for the response.

- No query strings yields the last 50 frames.
- if a `startframe` and `endframe` are both provided, returns frames between `startframe` and `endframe` inclusive. 
	- ex: `/frames?startframe=4&endframe=3`
- if a `numframes` is provided, provides the last `numframes` frames.
	- ex: `/frames?numframes=120` : returns the last 120 frames.
