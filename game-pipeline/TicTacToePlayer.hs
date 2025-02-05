module Main where

import Options.Applicative
import Debug.Trace
import Control.Monad
import System.Process
import System.IO
import System.Exit
import Data.Maybe
import Data.List
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import Types

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a


flushedPutStrLnB handle line = do
  B.hPutStrLn handle (LB.toStrict line)
  hFlush handle

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
