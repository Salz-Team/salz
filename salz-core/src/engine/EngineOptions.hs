{-# LANGUAGE OverloadedStrings #-}
module EngineOptions
  ( EngineCmdLineArgs(..)
  , parseInput
  , connectionString ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text
import Control.Exception
import System.Environment

data EngineCmdLineArgs
    = LocalArgs
        { dbFilePath  :: String
        , turnMax     :: Int
        , buildCmds   :: [String] }
    | ServerArgs
      deriving (Show)


localInput :: Parser EngineCmdLineArgs
localInput = LocalArgs
      <$> strOption
          ( long "databasepath"
         <> short 'd'
         <> help "Path where the sqlite game log will be stored" )
      <*> option auto
          ( long "turn"
         <> short 't'
         <> help "Number of turns to be computed" )
      <*> many (strOption
          ( long "players"
         <> short 'p'
         <> help ""))

serverInput :: Parser EngineCmdLineArgs
serverInput = flag' ServerArgs
  (  long "server"
  <> help "The mode used by the server" )

input :: Parser EngineCmdLineArgs
input = localInput <|> serverInput

parseInput :: IO EngineCmdLineArgs
parseInput = execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "The game engine for salz"
     <> header "salz-engine" )


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

