module Main where

import System.IO
import Data.Maybe
import Data.List
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import TicTacToe.Types
import Utils

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

play :: TicTacToeBoard -> TicTacToePlayerResponse
play board = fromJust $ do
  y <- findIndex (any isNothing) board
  x <- findIndex isNothing (board !! y)
  return $ TicTacToePlayerResponse x y

main :: IO ()
main = do
  board <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
  flushedPutStrLnB stdout $ encode $ play board
  main
