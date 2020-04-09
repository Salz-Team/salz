--------------------------------------------------------------------------------
-- Game Library
--------------------------------------------------------------------------------

module Entities where

import qualified Map as M


data Shape = S [(Int, Int)] deriving Show

instance Eq Shape where
  s1 == s2 = isSameShape s1 s2


unShape :: Shape -> [(Int, Int)]
unShape (S []) = []
unShape (S slst) = map (\(x, y) -> (x - minX, y - minY)) slst
  where
    minX = foldl1 min $ map fst slst
    minY = foldl1 min $ map snd slst


rotate :: Int -> Shape -> Shape
rotate times = S . (map (rotateCoord times)) . unShape

rotateCoord :: Int -> (Int, Int) -> (Int, Int)
rotateCoord times (x, y) = ( (round ((toEnum x) * (cos degrees) - (toEnum y) * (sin degrees)))
                     , (round ((toEnum x) * (sin degrees) + (toEnum y) * (cos degrees))))
  where
    degrees :: Float
    degrees = pi*(toEnum times)/2.0


isSameShape :: Shape -> Shape -> Bool
isSameShape s1 s2 = any id allPossibleMatches
  where
    allPossibleMatches = [(rotate r1 s1) ==  (rotate r2 s2) | r1 <- [0..3], r2 <- [0..3]]


data Instruction = Instr Shape [(Int, Int)] deriving Show

data Plan = Leaf (Maybe String) Shape | Node (Maybe String) Shape [(Plan, Instruction)]

getRotatedCmds :: Instruction -> Int -> [(Int, Int)]
getRotatedCmds (Instr (S slst) cmds) r = map (\(x, y) -> (x - minX, y - minY)) rcmds
  where
    rcmds = map (rotateCoord r) cmds
    rslst = map (rotateCoord r) slst
    minX = foldl1 min $ map fst rslst
    minY = foldl1 min $ map snd rslst


findShape :: [M.Coord] -> Shape -> [(M.Coord, Int)]
findShape world shp = filter isMatch possibleMatches
  where
    possibleMatches = [(coord, r) | coord <- world, r <- [0..3]]
    prepShape r = unShape $ rotate r shp
    firstOfPrep r = M.toCoord $ head $ prepShape r
    isMatch (coord, r) = all (\x -> elem (M.toCoord x + coord - firstOfPrep r) world) (prepShape r)

getCmds :: [M.Coord] -> Instruction -> [M.Coord]
getCmds world (Instr shp ucmds) = if findShape world shp == []
                                  then []
                                  else map (\x -> M.toCoord x + loc - first) cmds
  where
    (loc, r) = head  $ findShape world shp
    first = M.toCoord $ head $ unShape $ rotate r shp
    cmds = getRotatedCmds (Instr shp ucmds) r





-- data Shape = Shape {unShape :: [M.Coord]} deriving Show
-- data Pattern = Glider | Blinker | Point deriving Show
-- 
-- patternShapes :: Pattern -> [Shape]
-- patternShapes Glider  = [ Shape [M.C 1 0, M.C 2 1, M.C 0 2, M.C 1 2, M.C 2 2]
--                         , Shape [M.C 0 0, M.C 2 0, M.C 1 1, M.C 2 1, M.C 1 2]
--                         , Shape [M.C 2 0, M.C 0 1, M.C 2 1, M.C 1 2, M.C 2 2]
--                         , Shape [M.C 0 0, M.C 1 1, M.C 2 1, M.C 0 2, M.C 1 2]
--                         ]
-- patternShapes Blinker = [ Shape [M.C 0 0, M.C 0 1, M.C 0 2] ]
-- patternShapes Point = [ Shape [M.C 0 0] ]
-- 
-- 
-- 
-- data Heading = Heading { xSpeed :: Int, ySpeed :: Int}
-- 
-- data Plan = Leaf Shape | Node Shape [(Plan, [M.Coord])]
-- 
-- 
-- 
-- 
-- -- find all orientations of all shapes
-- -- then take the first live cell in each orientation and check agains every live cell
-- -- the result is the coord of the cell that corresponds to the first cell in the rotate
-- -- shape
-- identifyPattern :: Pattern -> [M.Coord] -> [(M.Coord, (Int, Shape))]
-- identifyPattern pat world = filter isMatch possibleMatches
--   where
--     shList = patternShapes pat
--     possibleShapes = [(r, shape) | r <- [0..3], shape <- shList]
--     possibleMatches = [(coord, sh) | coord <- world, sh <- possibleShapes]
--     prepShape (r, shape) = M.rotateCoordsAround (M.C 0 0) r (unShape shape)
--     isMatch (coord, rsh) = all (\x -> elem (x + coord - (head $ prepShape rsh)) world) (prepShape rsh)
