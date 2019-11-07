{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Control.Exception
import Data.Text
import Game

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs args =
  if args!!0 == "server"
  then connectionString >>= startServerGameEngine
  else startLocalGameEngine args


connectionString :: IO Text
connectionString = do
  envVars <- catch loadEnvVariables ((\_ -> pure []) :: IOError -> IO [Text])

  if envVars == []
  then
    return "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpassword'"
  else
    return $ "host='" `append` (envVars!!4) `append`
             "' port=" `append` (envVars!!3) `append`
             " dbname='" `append` (envVars!!0) `append`
             "' user='" `append` (envVars!!1) `append`
             "' password='" `append` (envVars!!2) `append`
             "'"

loadEnvVariables :: IO [Text]
loadEnvVariables = mapM (\x -> pack <$> getEnv x) ["POSTGRES_DB"
                                                  ,"POSTGRES_USER"
                                                  ,"POSTGRES_PASSWORD"
                                                  ,"POSTGRES_PORT"
                                                  ,"POSTGRES_HOST"]
