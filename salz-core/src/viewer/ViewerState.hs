{-# LANGUAGE DataKinds #-}
module ViewerState where
    
import Types

data ViewerState = ViewerState
  { turn :: Int
  , location :: (Int, Int)
  , dbfilepath :: FilePath
  , board :: [Limited (Board 100 100 Int)]
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

data Limited x = Limited MapArea TurnRange x
