module Main where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Data.Maybe
import System.IO
import TicTacToe.Types
import Utils

liftEither :: Either String a -> IO a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

play :: TicTacToeBoard -> TicTacToePlayerResponse
play board = fromJust $ do
  y <- L.findIndex (any isNothing) board
  x <- L.findIndex isNothing (board !! y)
  return $ TicTacToePlayerResponse x y

main :: IO ()
main = do
  board <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
  flushedPutStrLnB stdout $ encode $ play board
  main
