module Map ( MInt(..)
           , Coord(..)
           , Map(..)
           , rotateCoordsAround
           , toCoord
           ) where

import Text.Read
import Data.List
import Text.ParserCombinators.ReadP

mapSize :: Integer
mapSize = 100

--------------------------------------------------------------------------------
-- MInt
--------------------------------------------------------------------------------

data MInt = I {unMod :: Integer} deriving Eq

instance Show MInt where
  show (I a) = show a

instance Num MInt where
  fromInteger n = I (n `mod` mapSize)
  (I a) + (I b) = I ((a + b) `mod` mapSize)
  (I a) - (I b) = I ((a - b) `mod` mapSize)
  (I a) * (I b) = I ((a * b) `mod` mapSize)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Read MInt where
  readPrec = fmap fromInteger readPrec

--------------------------------------------------------------------------------
-- Coord
--------------------------------------------------------------------------------

data Coord = C { getX :: MInt, getY :: MInt }  deriving Eq

instance Show Coord where
  show (C x y) = show x ++ " " ++ show y

instance Num Coord where
  (C ax ay) + (C bx by) = C (ax + bx) (ay + by)
  (C ax ay) - (C bx by) = C (ax - bx) (ay - by)
  (C ax ay) * (C bx by) = C (ax * bx) (ay * by)
  fromInteger n = C (I n) (I n)
  abs    = undefined  -- make the warnings stop
  signum = undefined

instance Enum MInt where
  toEnum i = I $ toEnum i
  fromEnum (I a) = fromInteger $ a `mod` mapSize

instance Read Coord where
  readPrec = do
      x <- readPrec
      y <- readPrec
      return $ C x y

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
    rotateCoord (C x y) = C (I $ round ((toEnum $ fromEnum x) * (cos degrees) - (toEnum $ fromEnum y) * (sin degrees)))
                            (I $ round ((toEnum $ fromEnum x) * (sin degrees) + (toEnum $ fromEnum y) * (cos degrees)))

toCoord :: (Int, Int) -> Coord
toCoord (x, y) = C (toEnum x) (toEnum y)
