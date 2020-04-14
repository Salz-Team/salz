--------------------------------------------------------------------------------
-- Map Library
--------------------------------------------------------------------------------

module Map where

import Text.Read
import Data.List

mapSize :: Integer
mapSize = 100



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

rotateCoordsAround :: Coord -> Int -> [Coord] -> [Coord]
rotateCoordsAround pivot times = map (((+) pivot). rotateCoord . ((-) pivot))
  where
    degrees :: Float
    degrees = pi*(toEnum times)/2.0
    rotateCoord (C x y) = C (I $ round ((toEnum $ fromEnum x) * (cos degrees) - (toEnum $ fromEnum y) * (sin degrees)))
                            (I $ round ((toEnum $ fromEnum x) * (sin degrees) + (toEnum $ fromEnum y) * (cos degrees)))

toCoord :: (Int, Int) -> Coord
toCoord (x, y) = C (toEnum x) (toEnum y)


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


parseMap :: String -> [(Coord, Int)]
parseMap = format . map read . words
  where
    format :: [Int] -> [(Coord, Int)]
    format (x:y:p:rst) = (toCoord (x, y), p):(format rst)
    format [] = []

getMine :: Int -> [(Coord, Int)] -> [Coord]
getMine me world = map fst $ filter (\(_, i) -> i == me) world
