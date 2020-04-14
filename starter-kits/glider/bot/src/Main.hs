module Main where

import Text.Parsec

import qualified Map as M
import qualified Entities as E
import qualified PatternsParser as P
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.List
import System.IO


main :: IO ()
main = do
  raw <- getLine
  if "Initialize" == (head $ words raw)
    then initialize raw
    else takeTurn raw mapRequest think

data Memory = Memory { me :: Int
                     , center :: (Int, Int)
                     , turn :: Int
                     } deriving (Read, Show)

initialize :: String -> IO ()
initialize raw = do
  let [_, me_, x, y] = map read $ words raw
  let memory = Memory me_ (x, y) 0
  print memory
  hFlush stdout


takeTurn :: String -> (Memory -> (M.Coord, Int)) -> (Memory -> [(M.Coord, Int)] -> IO ([M.Coord], Memory))-> IO ()
takeTurn mem getReq cmdFun = do
  let memory = read mem :: Memory
  putStrLn $ show (fst $ getReq memory) ++ " " ++ show (snd $ getReq memory)
  hFlush stdout
  map_ <- M.parseMap <$> getLine
  (commands, memory') <- cmdFun memory map_
  let commandString = intercalate " " $ map show $ commands
  putStrLn $ "" ++ commandString
  hFlush stdout
  print memory'
  hFlush stdout

mapRequest :: Memory -> (M.Coord, Int)
mapRequest mem = (M.toCoord $ center mem, 100)


think :: Memory -> [(M.Coord, Int)] -> IO ([M.Coord], Memory)
think mem map_ = flip runStateT mem $ do
  modify (\m -> m {turn = (turn m) + 1})
  turn_ <- turn <$> get
  me_ <- me <$> get

  instructions <- lift P.readInstructions
  let blinkerInstr = P.getInstruction instructions "Blinker"
  let kBlinkerInstr = P.getInstruction instructions "Kill-blinker"
  let blowBlinkerInstr = P.getInstruction instructions "Blinker-to-4-blinkers"
  let blinkToGlider = P.getInstruction instructions "Blinker-to-glider"

  case turn_ of
    1 -> return $ E.getCmds (M.getMine me_ map_) (head blinkerInstr)
    2 -> return $ E.getCmds (M.getMine me_ map_) (blowBlinkerInstr!!0)
    3 -> return $ E.getCmds (M.getMine me_ map_) (blowBlinkerInstr!!1)
    otherwise -> return []
