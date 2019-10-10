{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game

main :: IO ()
main = startGameEngine "host='salz-db' port=5432 dbname='postgres' user='postgres' password='mysecretpassword'"
