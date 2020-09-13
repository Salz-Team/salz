module Map ( MInt(..)
           , Coord(..)
           , Map(..)
           , rotateCoordsAround
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

mapSize :: Integer
mapSize = 100

--------------------------------------------------------------------------------
-- MInt
--------------------------------------------------------------------------------

data MInt = MInt Integer deriving Eq

instance Show MInt where
  show (MInt a) = show $ a `mod` mapSize

instance Num MInt where
  fromInteger n = MInt (n `mod` mapSize)
  (MInt a) + (MInt b) = MInt ((a + b) `mod` mapSize)
  (MInt a) - (MInt b) = MInt ((a - b) `mod` mapSize)
  (MInt a) * (MInt b) = MInt ((a * b) `mod` mapSize)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Read MInt where
  readPrec = fmap fromInteger readPrec

instance Enum MInt where
  toEnum i = MInt $ (toEnum i) `mod` mapSize
  fromEnum (MInt a) = fromInteger $ a `mod` mapSize

--------------------------------------------------------------------------------
-- Coord
--------------------------------------------------------------------------------

data Coord = Coord { getX :: MInt, getY :: MInt }  deriving Eq

instance Show Coord where
  show (Coord x y) = show x ++ " " ++ show y

instance Num Coord where
  (Coord ax ay) + (Coord bx by) = Coord (ax + bx) (ay + by)
  (Coord ax ay) - (Coord bx by) = Coord (ax - bx) (ay - by)
  (Coord ax ay) * (Coord bx by) = Coord (ax * bx) (ay * by)
  fromInteger n = Coord (MInt n) (MInt n)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Read Coord where
  readPrec = do
      x <- readPrec
      y <- readPrec
      return $ Coord x y

--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

data Map = M [(Coord, Int)]

instance Show Map where
  show (M lst) = intercalate " " $ map (\(c, i) -> show c ++ " " ++ show i) lst

instance Read Map where
  readPrec = M <$> (lift $ many (readPrec_to_P readCoordInt 0))
    where
      readCoordInt :: ReadPrec (Coord, Int)
      readCoordInt = do
          c <- readPrec
          int <- readPrec
          return $ (c, int)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

rotateCoordsAround :: Coord -> Int -> [Coord] -> [Coord]
rotateCoordsAround pivot times = map (((+) pivot). rotateCoord . ((-) pivot))
  where
    degrees :: Float
    degrees = pi*(toEnum times)/2.0
    rotateCoord (Coord x y) = Coord (MInt $ round ((toEnum $ fromEnum x) * (cos degrees) - (toEnum $ fromEnum y) * (sin degrees)))
                            (MInt $ round ((toEnum $ fromEnum x) * (sin degrees) + (toEnum $ fromEnum y) * (cos degrees)))

toCoord :: (Int, Int) -> Coord
toCoord (x, y) = Coord (toEnum x) (toEnum y)

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord x y) = (fromEnum x, fromEnum y)

dist :: Coord -> Coord -> Int
dist (Coord ax ay) (Coord bx by) = max (min (fromEnum mapSize - fromEnum (ax - bx)) (fromEnum (ax - bx)))
                               (min (fromEnum mapSize - fromEnum (ay - by)) (fromEnum (ay - by)))

getRegion :: Map -> ((Int, Int), Int) -> Map
getRegion (M mlst) ((x, y), radius) = M $ filter (inRegion (Coord (toEnum x) (toEnum y)) radius) mlst
  where
    inRegion :: Coord -> Int -> (Coord, Int) -> Bool
    inRegion orig radius (x, _) = dist orig x <= radius

getNeighbours :: Map -> Coord -> [(Coord, Int)]
getNeighbours (M mlst) c = filter isNeighbour mlst
  where
    isNeighbour :: (Coord, Int) -> Bool
    isNeighbour (x, _) = dist c x == 1

getUniqueEmptyNeighbours :: Map -> [Coord]
getUniqueEmptyNeighbours (M mlst) = filter isEmpty $ nub $ concat $ map (getNeighbourCoords . fst) mlst
  where
    getNeighbourCoords :: Coord -> [Coord]
    getNeighbourCoords x = [x + disp | disp <- [toCoord (x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]  ]

    isEmpty :: Coord -> Bool
    isEmpty x = [] == (filter ((x ==) . fst) mlst)

getCellAt :: Map -> Coord -> Maybe Int
getCellAt (M mlst) x = snd <$> find (\(c,_) -> c == x ) mlst

getAlivePlayers :: Map -> [Int]
getAlivePlayers (M mlst) = nub $ map snd mlst

getPlayerSize :: Map -> Int -> Float
getPlayerSize (M mlst) pid = (toEnum numPlayerCells) / (toEnum numCells)
  where
    numPlayerCells = length $ filter (\(_, pid') -> pid == pid') mlst
    numCells = length mlst
