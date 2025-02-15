#!/usr/bin/env bash

USERBOTS=$(ppsql -t -c "select salz.users.id, salz.bots.upload_path, salz.bots.id from salz.users inner join salz.bots on salz.users.id=bots.user_id order by random() limit 2;" salz --csv)

TMPDIR=$(mktemp -d -p . gamerunXXX)
echo Tempdir: $TMPDIR

BOTPATHS=()
USER_IDS=()
BOT_IDS=()

for user in $USERBOTS
do
  USER_ID=$(echo $user | awk -F, '{print $1}')
  USER_IDS+=($(echo $user | awk -F, '{print $1}'))
  BOT_PATH=$(echo $user | awk -F, '{print $2}')
  BOT_IDS+=($(echo $user | awk -F, '{print $3}'))
  echo user_id: $USER_ID
  echo bot_path: $BOT_PATH
  echo bot_id: ${BOT_IDS[-1]}
  mkdir $TMPDIR/$USER_ID
  mc cp "local/salz/$BOT_PATH" "$TMPDIR/$USER_ID/bot.zip"
  unzip $TMPDIR/$USER_ID/bot.zip -d $TMPDIR/$USER_ID
  BOTPATHS+=($TMPDIR/$USER_ID/run.sh)
done

cabal run match-handler -- -g "cabal run tic-tac-toe" -b "bash ${BOTPATHS[0]}" -b "bash ${BOTPATHS[1]}" > $TMPDIR/gamefile

UPLOADPATH="gamefile/$(date -Ins).json"
echo $UPLOADPATH
mc cp "$TMPDIR/gamefile" "local/salz/$UPLOADPATH"

LASTMESSAGETYPE=$(jq -r --slurp '.[-1].messageType' $TMPDIR/gamefile)

if [ $LASTMESSAGETYPE = "gameEnd" ]
then
  STATUS="Finished"
else
  STATUS="Crashed"
fi

echo Status: $STATUS

SCORES=()

SCORES+=($(jq -r --slurp '.[-1].scores.[] | select(.player==0) | .score' $TMPDIR/gamefile))
SCORES+=($(jq -r --slurp '.[-1].scores.[] | select(.player==1) | .score' $TMPDIR/gamefile))

TIME=$(date -Iseconds)
GAMEID=$(ppsql -t -c "INSERT INTO salz.games (created_at, updated_at, status, upload_path) VALUES ('$TIME', '$TIME', '$STATUS', '$UPLOADPATH') RETURNING salz.games.id;" --csv | head -n1)

echo GameID: $GAMEID

ppsql -c "INSERT INTO salz.game_participants (game_id, user_id, bot_id, score, updated_at) VALUES ($GAMEID, ${USER_IDS[0]}, ${BOT_IDS[0]}, ${SCORES[0]}, '$TIME')"
ppsql -c "INSERT INTO salz.game_participants (game_id, user_id, bot_id, score, updated_at) VALUES ($GAMEID, ${USER_IDS[1]}, ${BOT_IDS[1]}, ${SCORES[1]}, '$TIME')"
