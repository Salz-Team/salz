{-# LANGUAGE DataKinds #-}
module ViewerState where
    
import Types

data ViewerState = ViewerState
  { turn :: Int
  , location :: (Int, Int)
  , dbfilepath :: FilePath
  , board :: (Board 100 100 CellInfo)
  , moves :: [(Int, Int, Int, PlayerId)]
  }

data MapArea = MapArea
  { topLeftx :: Int
  , topLefty :: Int
  , bottumRightx :: Int
  , bottumRighty :: Int
  } deriving (Show)

data TurnRange = TurnRange
  { firstTurn :: Int
  } deriving (Show)
