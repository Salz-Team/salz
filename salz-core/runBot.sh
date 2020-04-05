#!/bin/bash

if [ ! $# -eq 1 ]; then
  echo "Usage:"
  echo $0 "botdir"
  echo "Where botdir contains a directory \"bot\" which contains a bash script called"
  echo "\"run.sh\"."
  exit 1
fi

if [ ! -d ${1}/bot ]; then
  echo "Can't find the bot"
  exit 1
fi

if [ -e game.db ]; then
  echo "Can't run game as there is already a \"game.db\" database"
  exit 1
fi

echo "Setting up game"

pushd $1 > /dev/null
tar cvf bot.tar.gz bot > /dev/null
botdir=$(pwd)
popd > /dev/null


echo "Running game..."
stack exec salz-engine -- -d "$(pwd)/game.db" -p ${botdir}/bot.tar.gz -t 200 > /dev/null &
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
