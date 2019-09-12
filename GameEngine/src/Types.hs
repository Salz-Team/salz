{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Types where

import Data.Modular
import ExternalProcessHandler
import GHC.TypeLits hiding (Mod)
import qualified Data.Text as T


data Game w h = Game
  { board :: Board w h CellInfo
  , players :: [(Player, T.Text)]
  , turn :: Int
  , dbconnstring :: T.Text
  }

data Cell (w :: Nat) (h :: Nat) i = Cell
  { cX :: Mod Int w
  , cY :: Mod Int h
  , cItem :: i
  } deriving (Show)

data Board w h i = Board
  { bCells :: [Cell w h i]
  } deriving (Show)

type PlayerId = Int

data Player = Player
  { pPlayerId :: PlayerId
  , pPlayerSource :: (Int, Int)
  } deriving (Show)

data CellInfo = CellInfo
  { cPlayerId :: PlayerId
  }

data Command = Flip Int Int
  deriving Show

type PlayerMap = [(Int, Int, PlayerId)]
data PlayerBotHandler = PlayerBotHandler
  { eph :: ExternalProcessHandler
  } 

data ParseError = OddNumOfCoordinates | NonIntCoordinate |  Extern Error
  deriving Show
