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
  let gliderToGliders = P.getInstruction instructions "Glider-to-gliders"
  let blinkerToGliders = P.getInstruction instructions "Blinker-to-2-gliders"

  case turn_ of
    1 -> return $ E.getCmds (M.getMine me_ map_) (head blinkerInstr)

    2 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!0)
    3 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!1)
    4 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!2)
    5 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!3)
    6 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!4)
    7 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!5)
    8 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!6)
    9 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!7)
    10 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!8)
    11 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!9)
    12 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!10)
    13 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!11)
    14 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!12)
    15 -> return $ E.getCmds (M.getMine me_ map_) (blinkerToGliders!!13)

    21 -> do
        let (E.Instr shp _) = gliderToGliders!!0
        lift $ hPutStrLn stderr $ show $ length (E.findShape (M.getMine me_ map_) shp)
        lift $ hFlush stderr
        return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!0)
    22 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!1)
    23 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!2)
    24 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!3)
    25 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!4)
    26 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!5)
    27 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!6)
    28 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!7)
    29 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!8)
    30 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!9)
    31 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!10)
    32 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!11)
    33 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!12)
    34 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!13)
    35 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!14)
    36 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!15)

    42 -> do
        let (E.Instr shp _) = gliderToGliders!!0
        lift $ hPutStrLn stderr $ show $ length (E.findShape (M.getMine me_ map_) shp)
        lift $ hFlush stderr
        return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!0)
    43 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!1)
    44 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!2)
    45 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!3)
    46 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!4)
    47 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!5)
    48 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!6)
    49 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!7)
    50 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!8)
    51 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!9)
    52 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!10)
    53 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!11)
    54 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!12)
    55 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!13)
    56 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!14)
    57 -> return $ E.getCmds (M.getMine me_ map_) (gliderToGliders!!15)

    otherwise -> return []
