{-# LANGUAGE OverloadedStrings #-}
module ViewerOptions
  ( ViewerCmdLineArgs(..)
  , parseInput ) where

import Options.Applicative

data ViewerCmdLineArgs = ViewerCmdLineArgs
  { dbFilePath :: String
  } deriving (Show)


input :: Parser ViewerCmdLineArgs
input = ViewerCmdLineArgs
  <$> strOption
       ( long ""
     <> short 'd'
     <> help "Path where the sqlite game log is stored" )

parseInput :: IO ViewerCmdLineArgs
parseInput = execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "The terminal viewer for salz"
     <> header "salz-terminal-viewer" )


