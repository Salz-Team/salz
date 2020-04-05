module Step where

import Board
import Types

import Data.Maybe
import Data.List
import GHC.TypeLits hiding (Mod)


step :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> Board h w CellInfo
step b = b { bCells = newCells}
  where
    newCells = (filterLiveCells b) ++ (produceNewCells b)

filterLiveCells :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> [Cell h w CellInfo]
filterLiveCells b = filter (healthy b) (bCells b)
  where
    healthy b1 c = elem (numNeighbours b1 c) [2, 3]

produceNewCells :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> [Cell h w CellInfo]
produceNewCells b = catMaybes $ produceCells b
  where
    produceCells :: (KnownNat w, KnownNat h) =>
      Board h w CellInfo -> [Maybe (Cell h w CellInfo)]
    produceCells b1 = map (maybeBorn b1) $ getUniqueEmptyNeighbours b1 (bCells b1)
    
    maybeBorn :: (KnownNat w, KnownNat h) =>
      Board h w CellInfo -> Cell h w a -> Maybe (Cell h w CellInfo)
    maybeBorn b1 c
      | isBorn b1 c = Just $ fillCell (CellInfo (newOwner b1 c)) c
      | otherwise = Nothing
    
    
    isBorn :: (KnownNat w, KnownNat h) =>
      Board h w CellInfo -> Cell h w a -> Bool
    isBorn b1 c = elem (numNeighbours b1 c) [3]
    
    
    newOwner :: (KnownNat w, KnownNat h) =>
      Board h w CellInfo -> Cell h w a -> PlayerId
    newOwner b1 c = most $  map (cPlayerId . cItem) $ getNeighbours b1 c

most :: (Eq a) => [a] -> a
most l = snd $ head $ sortBy (\a b -> fst a `compare` fst b) occurences
  where
    occurences = map (\x -> (length $ findIndices (x ==) l, x)) uniq
    uniq = nub l
