{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Types where

import Data.Modular
import ExternalProcessHandler
import GHC.TypeLits hiding (Mod)
import qualified Data.Text as T
import qualified Data.Either as E

data Cell (w :: Nat) (h :: Nat) i = Cell
  { cX :: Mod Int w
  , cY :: Mod Int h
  , cItem :: i
  } deriving (Show)

instance Eq (Cell w h i) where
  a == b = (cX a == cX b) && (cY a == cY b)

data Board w h i = Board
  { bCells :: [Cell w h i]
  } deriving (Show)

type PlayerId = Int

data Player = Player
  { pPlayerId :: PlayerId
  , pBotHandler :: BotHandler
  }

data CellInfo = CellInfo
  { cPlayerId :: PlayerId
  }

instance Show CellInfo where
    show (CellInfo pid) = show pid

data Command = Flip Int Int
  deriving Show

type PlayerMap = [(Int, Int, PlayerId)]

data BotHandler = BotHandler
  { eph :: E.Either T.Text ExternalProcessHandler
  }


