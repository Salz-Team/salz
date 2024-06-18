#!/bin/bash

while true
do
  sleep 1
  bash ./match-maker.sh | xargs bash ./match-handler.sh | xargs bash ./ranker.sh
done
