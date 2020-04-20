{-# LANGUAGE DataKinds #-}
module ViewerState where

import qualified Map as Map

data ViewerState = ViewerState
  { turn :: Int
  , play :: Bool
  , location :: (Int, Int)
  , dbfilepath :: FilePath
  , board :: Map.Map
  , moves :: [(Int, Int, Int, Int)]
  , errlogs :: Maybe [(Int, String, String, String)]
  }
