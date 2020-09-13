#!/bin/bash



if [ ! $# -ge 2 ]; then
  echo "Usage:"
  echo $0 "[botdir]" "nturn"
  echo "Where botdir contains a directory \"bot\" which contains a bash script called"
  echo "\"run.sh\", and nturn is the number of turns to run."
  exit 1
fi

BOTS=${@:1:$#-1}
NTURNS=${@:$#:$#}

for bot in $BOTS
do
  if [ ! -d ${bot}/bot ]; then
    echo "Can't find the bot at ${bot}"
    exit 1
  fi
done

if [ -e game.db ]; then
  echo "Can't run game as there is already a \"game.db\" database"
  exit 1
fi

echo "Setting up game"

for bot in $BOTS
do
  pushd $bot > /dev/null
  tar cvf bot.tar.gz bot > /dev/null
  botdirs+=" -p $(pwd)/bot.tar.gz "
  popd > /dev/null
done
 
echo "Running game..."
stack exec salz-engine -- -d "$(pwd)/game.db" ${botdirs[@]} -t $NTURNS > /dev/null &
enginepid=$!

spin='-\|/'

i=0
while kill -0 $enginepid 2>/dev/null
do
  i=$(( (i+1) %4 ))
  printf "\r${spin:$i:1}"
  sleep .1
done

echo

stack exec salz-viewer -- -d "$(pwd)/game.db"

echo "Cleaning up files"

rm game.db
pushd $1 > /dev/null
rm bot.tar.gz
popd > /dev/null
