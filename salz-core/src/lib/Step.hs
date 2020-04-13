module Step (step) where

import qualified Map as Map
import Data.Maybe
import Data.List


step :: Map.Map -> Map.Map
step (Map.M oldlst) = Map.M $ (filterLiveCells oldlst) ++ (produceNewCells (Map.M oldlst))


filterLiveCells :: [(Map.Coord, Int)] -> [(Map.Coord, Int)]
filterLiveCells oldm = filter isHealthy oldm
  where
    isHealthy :: (Map.Coord, Int) -> Bool
    isHealthy c = elem (length $ Map.getNeighbours (Map.M oldm) (fst c)) [2, 3]

produceNewCells :: Map.Map -> [(Map.Coord, Int)]
produceNewCells oldm = catMaybes $ potentialNewCells
  where
    potentialNewCells = map maybeBorn $ Map.getUniqueEmptyNeighbours oldm
    
    maybeBorn :: Map.Coord -> Maybe (Map.Coord, Int)
    maybeBorn c
      | isBorn c = Just (c, newOwner c)
      | otherwise = Nothing
    
    
    isBorn :: Map.Coord -> Bool
    isBorn c = elem (length $ Map.getNeighbours oldm c) [3]
   
    
    newOwner :: Map.Coord -> Int
    newOwner c = most $  map snd $ Map.getNeighbours oldm c

most :: (Eq a) => [a] -> a
most l = snd $ head $ sortBy (\a b -> fst b `compare` fst a) occurences
  where
    occurences = map (\x -> (length $ findIndices (x ==) l, x)) uniq
    uniq = nub l
