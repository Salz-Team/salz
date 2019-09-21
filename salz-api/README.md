# salz-api

## Endpoints

Endpoints currently implemented

### `/login`

Accessing the `/login` endpoint challenges the user's identity via Github. The callback for this is `/login/auth` which returns a JWT token.

The user visits `/login`, gets redirected to Github login, then redirected again to `/login/auth`

###  `/login/auth`

This endpoint is only for OAuth. After the user's identity is verified, an access token is sent back with this endpoint. We don't need to actually maintain access to the Github profile data. We encode the username, email, and JWT expiry time into a JWT token. The token is sent back in the following form:

```
{
	token: "eyj0eXAiOjk...."
}
```

Requests that require authorization via JWT must include an authorization header with the JWT token specifying the bearer schema. E.g.:

```
... headers
Authorization: Bearer ejy0eXAi0jk....
... more headers
```

Tokens currently set to expire after 30 minutes, subject to change.

### `/user`

Protected endpoint. Requires a signed JWT bearer token. 

Returns user data in the following form (more to be added later, probably)

```
{
	"email" : "mrpanino420@loo.ca",
	"exp" : 42042069,
	"login" : "feridun"
}
```

where the email, login properties are the email and username of the Github account.


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
