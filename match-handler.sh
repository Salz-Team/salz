#!/usr/bin/env bash

players[0]=$1
players[1]=$2

bots[0]=$(psql -c "select botfile from users where userid=${players[0]};" salz --csv | sed '1d')
bots[1]=$(psql -c "select botfile from users where userid=${players[1]};" salz --csv | sed '1d')

tmpdir=$(mktemp -d)

pushd $tmpdir > /dev/stderr
  mc cp local/bots/${bots[0]} bot0.sh > /dev/stderr
  mc cp local/bots/${bots[1]} bot1.sh > /dev/stderr
  out0=$(bash bot0.sh)
  out1=$(bash bot1.sh)
  game=$(uuidgen)
  echo $out0 >> $game
  echo $out1 >> $game
  mc cp $game local/games/$game > /dev/stderr
popd > /dev/stderr
rm -r $tmpdir > /dev/stderr

if [[ $out0 == $out1 ]]; then
  winner=${players[0]}
elif [[ $out0 ==  "rock" && $out1 == "scissors" ]]; then
  winner=${players[0]}
elif [[ $out0 == "sissors" && $out1 == "rock" ]]; then
  winner=${players[1]}
elif [[ $out0 == "scissors" && $out1 == "paper" ]]; then
  winner=${players[0]}
elif [[ $out0 == "paper" && $out1 == "scissors" ]]; then
  winner=${players[1]}
elif [[ $out0 == "paper" && $out1 == "rock" ]]; then
  winner=${players[0]}
elif [[ $out0 = "rock" && $out1 == "paper" ]]; then
  winner=${players[1]}
else
  echo "There was an error please try again" > /dev/stderr
fi

psql -c "insert into games values (default, ${players[0]}, ${players[1]}, $winner, '"$(date -I'seconds')"','$game');" salz > /dev/stderr
echo winner is $winner > /dev/stderr
echo $winner
