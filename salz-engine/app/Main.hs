{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game

main :: IO ()
main = startGameEngine "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpasswrd'"
