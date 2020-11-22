{-# LANGUAGE OverloadedStrings #-}
module Step ( step, getAround, dist, thrd ) where

import qualified Database.PostgreSQL.Simple as DB
import Data.List
import qualified Data.Maybe as M
import Database

step :: DB.Connection -> IO ()
step conn = do
  turn <- getMostRecentTurn conn
  lastFrame <- getFrame conn turn
  let newFrame = stepFunction lastFrame
  putFrame conn turn newFrame

--------------------------------------------------------------------------------
-- Game Step Function
--------------------------------------------------------------------------------
mapSize = 1000

stepFunction :: [(Int, Int, Int)] -> [(Int, Int, Int)]
stepFunction frame = (filterLiveCells frame) ++ (produceNewCells frame)

filterLiveCells :: [(Int, Int, Int)] -> [(Int, Int, Int)]
filterLiveCells frame = filter (healthy frame) frame
  where
    healthy frame cell = elem (length $ getAround frame 1 cell) [3, 4]

produceNewCells :: [(Int, Int, Int)] -> [(Int, Int, Int)]
produceNewCells frame = filter (not . (\x -> elem x frame)) newCells
  where
    newCells = map (\(x, y, _) -> (x, y, cellOwner (x, y, -1))) newCellPositions
    newCellPositions = filter healthy uniqueEmptyNeighbours
    uniqueEmptyNeighbours = nub $ concat $ map neighbourPositions frame
    neighbourPositions (x, y, _) = [(mod (x + gx) mapSize, mod (y + gy) mapSize, -1) | gx <- [-1..1], gy <- [-1..1], gx /= 0 || gy /= 0]
    healthy cell = elem (length $ filter (/= cell) $ getAround frame 1 cell) [3]
    cellOwner cell = M.fromJust $ M.fromJust $ find M.isJust (map (\radius -> mode  (map thrd $ getAround frame radius cell)) [1..100])

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

mode :: Eq a => [a] -> Maybe a
mode a = if maxa == maxb then (Just maxa) else Nothing
  where
    maxa = head $ maximumBy (\x y -> compare (length x) (length y)) $ groupBy (==) a
    maxb = head $ maximumBy (\x y -> compare (length x) (length y)) $ groupBy (==) $ reverse a

getAround :: [(Int, Int, Int)] -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
getAround frame range cell = filter (\x -> dist cell x <= range) frame

dist :: (Int, Int, Int) -> (Int, Int, Int) -> Int
dist (ax, ay, _) (bx, by, _) = max (ddist ax bx) (ddist ay by)
  where
    ddist :: Int -> Int -> Int
    ddist a b = min (abs ((mod a mapSize) - (mod b mapSize))) (abs ((mod b mapSize) - (mod a mapSize)))
