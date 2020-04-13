module Player ( applyCommands
              ) where

import qualified Map as Map
import Data.List

applyCommands :: Map.Map -> [(Map.Coord, Int)] -> Map.Map
applyCommands map cmds = foldl applyCommand map $ filterLegalCommands map cmds
  where
    applyCommand :: Map.Map -> (Map.Coord, Int) -> Map.Map
    applyCommand (Map.M mlst) cel = if length (delete cel mlst) /= length mlst
                                    then Map.M $ cel:mlst
                                    else Map.M $ delete cel mlst

filterLegalCommands :: Map.Map -> [(Map.Coord, Int)] -> [(Map.Coord, Int)]
filterLegalCommands map cmds = filter (isCommandLegal map) cmds

isCommandLegal :: Map.Map -> (Map.Coord, Int) -> Bool
isCommandLegal map (c, pid) = isPlayerNeighbour map pid c && (not $ isNearEnemy map pid c)

isPlayerNeighbour :: Map.Map -> Int -> Map.Coord -> Bool
isPlayerNeighbour map pid c = any (\(x, _) -> Map.dist x c <= 1) $ getPlayerCells map pid

isNearEnemy :: Map.Map -> Int -> Map.Coord -> Bool
isNearEnemy map pid c = any (\(x, _) -> Map.dist x c <= 30) $ getOtherPlayerCells map pid

getPlayerCells :: Map.Map -> Int -> [(Map.Coord, Int)]
getPlayerCells (Map.M mlst) pid = filter (\(_, x) -> x == pid) mlst

getOtherPlayerCells :: Map.Map -> Int -> [(Map.Coord, Int)]
getOtherPlayerCells (Map.M mlst) pid = filter (\(_, x) -> x /= pid) mlst
