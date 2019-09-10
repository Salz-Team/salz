{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Board where

import Types

import Data.Modular
import Data.Maybe
import Data.List
import GHC.TypeLits hiding (Mod)

instance Eq (Cell w h i) where
  a == b = (cX a == cX b) && (cY a == cY b)

numNeighbours :: (KnownNat w, KnownNat h) 
  => Board w h i -> Cell w h j -> Int
numNeighbours b c = length $ getNeighbours b c

getNeighbours :: (KnownNat w, KnownNat h) 
  => Board w h i -> Cell w h j -> [Cell w h i]
getNeighbours b c = filter (isNeighbour c) (bCells b)

isNeighbour :: (KnownNat w, KnownNat h) 
  => Cell w h i -> Cell w h j -> Bool
isNeighbour a b
  | cDist (cX a) (cX b) == 1 && cDist (cY a) (cY b) == 0 = True
  | cDist (cY a) (cY b) == 1 && cDist (cX a) (cX b) == 0 = True
  | cDist (cY a) (cY b) == 1 && cDist (cX a) (cX b) == 1 = True
  | otherwise                                     = False

getEmptyNeighbours :: (KnownNat w, KnownNat h) 
  => Board w h i -> Cell w h a -> [Cell w h ()]
getEmptyNeighbours b c =
  let
    cellNotExists :: Board w h i -> Cell w h a -> Bool
    cellNotExists b1 x = not $ any (mEq x) $ bCells b1
    
    neighbourCells :: (KnownNat w, KnownNat h) 
      => Board w h i -> Cell w h a -> [Cell w h ()]
    neighbourCells _ c1 = do
      x <- [(unMod $ cX c1)-1..(unMod $ cX c1)+1]
      y <- [(unMod $ cY c1)-1..(unMod $ cY c1)+1]
      return (Cell (toMod x) (toMod y) ())
  in
    filter (cellNotExists b) (neighbourCells b c)

getUniqueEmptyNeighbours :: (KnownNat w, KnownNat h) 
  => Board w h i -> [Cell w h a] -> [Cell w h ()]
getUniqueEmptyNeighbours b cl = nub $ concat $ map (getEmptyNeighbours b) cl

fillCell :: i -> Cell w h a -> Cell w h i
fillCell a (Cell x y _) = Cell x y a

mEq :: Cell w h a -> Cell w h b -> Bool
mEq a b = (fillCell () a) == (fillCell () b)

relativeCoordinates :: (KnownNat w, KnownNat h) =>
  Cell w h a -> Cell w h b -> Cell w h ()
relativeCoordinates origin target = Cell nx ny ()
  where
    nx = cSignedDist (cX origin) (cX target)
    ny = cSignedDist (cY origin) (cY target)

cDist :: (KnownNat n, Integral i)
  => Mod i n -> Mod i n -> Mod i n
cDist a b = min (abs (a - b)) (abs (b - a))

cSignedDist :: (KnownNat n, Integral i)
  => Mod i n -> Mod i n -> Mod i n
cSignedDist a b = b - a

dist :: (KnownNat w, KnownNat h)
  => Cell w h i -> Cell w h j -> Int
dist a b = unMod (cDist (cX a) (cX b)) + unMod (cDist (cY a) (cY b))

getCellAt :: Board w h a -> Cell w h b -> Maybe (Cell w h a)
getCellAt b c = find (mEq c) (bCells b)

getCellsBy :: Board w h a -> (a -> Bool) -> [(Cell w h a)]
getCellsBy b f = filter (f . cItem) (bCells b)

display :: (Show i, KnownNat w, KnownNat h) 
  => Board w h i -> Int -> Int -> Int -> Int -> Maybe String
display b vx vy sx sy
  | vx <= 0 && vy <= 0 = Nothing
  | otherwise          = Just $ concat $ concat $ map displayRow [vx..(vx+sx)]
  where
    displayRow r = (map (displayi r) [vy..(vy+sy)]) ++ ["\n"]

    displayi :: Int -> Int -> String
    displayi y x = fromMaybe " " (cdisplay <$> getCellAt b (Cell (toMod x) (toMod y) ()))

    cdisplay :: Show i => Cell w h i -> String
    cdisplay (Cell _ _ i) = show i

toggleCell :: Board w h i -> Cell w h i -> Board w h i
toggleCell b c = case getCellAt b c of
                   Nothing -> b {bCells = c:(bCells b)}
                   Just t -> b {bCells = filter (not . mEq t) (bCells b)}

