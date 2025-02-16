module TicTacToe.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics

data TicTacToePlayerResponse = TicTacToePlayerResponse Int Int deriving (Show)
type TicTacToeBoard = [[Maybe TicTacToePlayer]]
data TicTacToePlayer = X | O deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromJSON TicTacToePlayerResponse where
  parseJSON = withObject "PlayerResponse" $ \obj -> do
    [x, y] <- obj .: "move"
    if inRange x y
      then
        return (TicTacToePlayerResponse x y)
      else
        fail "Coordinates out of bounds"
    where
      inRange :: Int -> Int -> Bool
      inRange x y = x >= 0 && x <= 2 && y >= 0 && y <= 2

instance ToJSON TicTacToePlayerResponse where
  toJSON (TicTacToePlayerResponse x y) = object [
    "move" .= [x, y]
    ]

