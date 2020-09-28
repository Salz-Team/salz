module Map ( ModularInt(..)
           , Coord(..)
           , Map(..)
           , getPlayerSize
           , getAlivePlayers
           , toCoord
           , getRegion
           , dist
           , getNeighbours
           , getUniqueEmptyNeighbours
           , getCellAt
           ) where

import Text.Read
import Data.List
import Text.ParserCombinators.ReadP

mapSize :: Int
mapSize = 100

--------------------------------------------------------------------------------
-- ModularInt
--------------------------------------------------------------------------------
-- The map wraps (it is a torus) so we need modular integers.

newtype ModularInt = ModularInt Int deriving Eq

instance Show ModularInt where
  show (ModularInt a) = show $ a `mod` mapSize

instance Num ModularInt where
  fromInteger n = ModularInt (fromEnum n `mod` mapSize)
  (ModularInt a) + (ModularInt b) = ModularInt ((a + b) `mod` mapSize)
  (ModularInt a) - (ModularInt b) = ModularInt ((a - b) `mod` mapSize)
  (ModularInt a) * (ModularInt b) = ModularInt ((a * b) `mod` mapSize)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Read ModularInt where
  readPrec = fmap fromInteger readPrec

instance Enum ModularInt where
  toEnum i = ModularInt (toEnum i `mod` mapSize)
  fromEnum (ModularInt a) = a `mod` mapSize

--------------------------------------------------------------------------------
-- Coord
--------------------------------------------------------------------------------

data Coord = Coord { getX :: ModularInt, getY :: ModularInt }  deriving Eq

instance Show Coord where
  show (Coord x y) = show x ++ " " ++ show y

instance Num Coord where
  (Coord ax ay) + (Coord bx by) = Coord (ax + bx) (ay + by)
  (Coord ax ay) - (Coord bx by) = Coord (ax - bx) (ay - by)
  (Coord ax ay) * (Coord bx by) = Coord (ax * bx) (ay * by)
  fromInteger n = Coord (ModularInt $ fromEnum n) (ModularInt $ fromEnum n)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Read Coord where
  readPrec = Coord <$> readPrec <*> readPrec

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

newtype Map = Map [(Coord, Int)]

instance Show Map where
  show (Map lst) = unwords $ map (\(c, i) -> show c ++ " " ++ show i) lst

instance Read Map where
  readPrec = Map <$> lift (many (readPrec_to_P readCoordInt 0))
    where
      readCoordInt :: ReadPrec (Coord, Int)
      readCoordInt = do
          c <- readPrec
          int <- readPrec
          return (c, int)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

toCoord :: (Int, Int) -> Coord
toCoord (x, y) = Coord (toEnum x) (toEnum y)

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord x y) = (fromEnum x, fromEnum y)

modAbs :: ModularInt -> Int
modAbs a = min (fromEnum a) (mapSize - fromEnum a)

dist :: Coord -> Coord -> Int
dist (Coord ax ay) (Coord bx by) = max (modAbs (ax -by)) (modAbs (ay -by))

getRegion :: Map -> ((Int, Int), Int) -> Map
getRegion (Map cellList) ((x, y), radius) = Map $ filter (inRegion (toCoord (x,y)) radius) cellList
  where
    inRegion :: Coord -> Int -> (Coord, Int) -> Bool
    inRegion orig radius (x, _) = dist orig x <= radius

getNeighbours :: Map -> Coord -> [(Coord, Int)]
getNeighbours (Map cellList) c = filter isNeighbour cellList
  where
    isNeighbour :: (Coord, Int) -> Bool
    isNeighbour (x, _) = dist c x == 1

getUniqueEmptyNeighbours :: Map -> [Coord]
getUniqueEmptyNeighbours (Map cellList) = filter isEmpty $ nub $ concatMap (getNeighbourCoords . fst) cellList
  where
    getNeighbourCoords :: Coord -> [Coord]
    getNeighbourCoords x = [x + disp | disp <- [toCoord (x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]  ]

    isEmpty :: Coord -> Bool
    isEmpty x = [] == filter ((x ==) . fst) cellList

getCellAt :: Map -> Coord -> Maybe Int
getCellAt (Map cellList) x = snd <$> find (\(c,_) -> c == x ) cellList

getAlivePlayers :: Map -> [Int]
getAlivePlayers (Map cellList) = nub $ map snd cellList

getPlayerSize :: Map -> Int -> Float
getPlayerSize (Map cellList) playerId = numPlayerCells / numCells
  where
    numPlayerCells = toEnum $ length $ filter (\(_, playerId') -> playerId == playerId') cellList
    numCells = toEnum $ length cellList
