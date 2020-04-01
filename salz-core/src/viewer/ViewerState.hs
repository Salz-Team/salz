{-# LANGUAGE DataKinds #-}
module ViewerState where
    
import Types

data ViewerState = ViewerState
  { turn :: Int
  , play :: Bool
  , location :: (Int, Int)
  , dbfilepath :: FilePath
  , board :: (Board 100 100 CellInfo)
  , moves :: [(Int, Int, Int, PlayerId)]
  }

