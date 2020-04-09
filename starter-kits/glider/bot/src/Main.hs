module Main where

import Text.Parsec

import qualified Map as M
import qualified Entities as E
import qualified PatternsParser as P


main :: IO ()
main = do

  -- Read in txt files
  shapes <- P.readShapes
  instructions <- P.readInstructions

  -- Make blinker
  let blinkerInstr = P.getInstruction instructions "Blinker"
  let blowBlinkerInstr = P.getInstruction instructions "Blinker-to-4-blinkers"

  -- Who am I
  me <- minit

  mapM_ (\instr -> M.takeTurn (\world -> E.getCmds (M.getMine me world) instr)) blinkerInstr

  mapM_ (\instr -> M.takeTurn (\world -> E.getCmds (M.getMine me world) instr)) blowBlinkerInstr


  forever


forever :: IO ()
forever = do
  M.takeTurn (\_ -> [])
  forever

minit = do
  raw <- getLine
  putStrLn ""
  return $ read $ (words raw)!!0
