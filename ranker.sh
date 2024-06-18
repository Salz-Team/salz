#!/usr/bin/env bash

winner=$1

psql -c "update users set elo = elo + 1 where userid = $winner" salz

echo Winner is $winner
