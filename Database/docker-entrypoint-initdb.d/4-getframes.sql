CREATE OR REPLACE FUNCTION get_latest_turnid()
	RETURNS INTEGER
AS $$
DECLARE
	maxturn INTEGER;
BEGIN
	SELECT max(Game.turnid)
	INTO maxturn
	FROM Game;

	RETURN maxturn;
END; $$
LANGUAGE 'plpgsql';

CREATE OR REPLACE FUNCTION get_playerframes(startFrame INTEGER, endFrame INTEGER) 
	RETURNS TABLE ( 
		turnid INTEGER,
		playerid INTEGER,
		pos JSON)
AS $$
BEGIN
	RETURN QUERY SELECT
		Game.turnid,
		Game.playerid,
		json_agg(json_build_object('x', Game.x, 'y', Game.y))
	FROM Game 
	WHERE Game.turnid BETWEEN startFrame AND endFrame
	GROUP BY Game.turnid, Game.playerid;
END; $$
LANGUAGE 'plpgsql';

CREATE OR REPLACE FUNCTION get_frames(startFrame INTEGER, endFrame INTEGER) 
	RETURNS TABLE ( 
		frames JSON)
AS $$
BEGIN
	RETURN QUERY SELECT
		json_build_object(K.turnid, json_agg(K)) "frames"
		FROM (SELECT * FROM get_playerframes(startFrame, endFrame)) K
	GROUP BY K.turnid
	ORDER BY K.turnid;
END; $$
LANGUAGE 'plpgsql';
