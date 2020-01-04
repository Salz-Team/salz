{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import EngineOptions

main :: IO ()
main = do
    args <- parseInput
    startGameEngine args

