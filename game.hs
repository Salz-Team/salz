import Data.List
import System.Environment
import System.Process
import System.IO
import GHC.IO.Handle

data MapSize = MapSize Int Int

data Cell = Cell 
  {xc :: Int
  ,yc :: Int
  } deriving (Eq)

instance Show Cell where
  show (Cell x y) = unwords $ map show [x,y]

data Map = Map 
  { cellsm ::  [Cell]
  , sizem :: MapSize
  , turnm :: Int
  }


-- Get Input

main = do
  args <- getArgs
  if length args /= 4
  then error $ "Usage:\n game  w h play1 play2"
  else return ()

  let mw = read $ args!!0
  let mh = read $ args!!1
  putStrLn $ args!!0 ++ " " ++ args!!1
  let p1path = args!!2
  let p2path = args!!3

  (stdin1, stdout1, stderr1, proc1) <- runInteractiveCommand p1path
  (stdin2, stdout2, stderr2, proc2) <- runInteractiveCommand p2path

  hPutStr stdin1 (show mh ++ " " ++ show mw ++ "\n")  
  hPutStr stdin2 (show mh ++ " " ++ show mw ++ "\n")  
  hFlush stdin1
  hFlush stdin2
  
  name1 <- hGetLine stdout1
  name2 <- hGetLine stdout2
  putStrLn (name1 ++ " " ++ name2)
 
  let gamemap = Map [] (MapSize mw mh) 0
  
  gameTurn stdin1 stdin2 stdout1 stdout2 gamemap

  return ()
  
gameTurn stdin1 stdin2 stdout1 stdout2 gamemap = do
  play1raw <- hGetLine stdout1
  play2raw <- hGetLine stdout2
  let play1 = map (\b -> Cell 1 b) $ map read $ words play1raw
  let play2 = map (\b -> Cell 1 b) $ map read $ words play2raw
  let MapSize mw mh = sizem gamemap
  let fplay1 = filter (\(Cell x y) -> x == 1 && y <= mh && y > 0) play1
  let fplay2 = map (\(Cell _ y) -> (Cell mw (mh + 1 - y))) $ filter (\(Cell x y) -> x == 1 && y > 0 && y <= mh) play2
  let m = cellsm gamemap
  let ncels = invertelm (invertelm m fplay2) fplay1
  let pigamemap = gamemap {cellsm = ncels}
  let ngamemap = stepGame pigamemap
  hPutStr stdin1 $ (unwords $ map show $ cellsm ngamemap) ++ "\n"
  hPutStr stdin2 $ (unwords $ map show $ flipmap ngamemap) ++ "\n"
  hFlush stdin1
  hFlush stdin2
  putStrLn $ unwords $ map show $ cellsm ngamemap
--  print pigamemap
--  print ngamemap
  if isWin ngamemap == 0
  then gameTurn stdin1 stdin2 stdout1 stdout2 ngamemap
  else return () -- print $ isWin ngamemap

isWin gameMap 
  | p1 > 0 && p2 > 0 = 3
  | p1 > 0 = 1
  | p2 > 0 = 2
  | t >= 1000 = 3
  | otherwise = 0
  where
    p1 = length $ filter (\(Cell x y) -> x == (div mw 3) &&  y == (div mh 3)) $ cellsm gameMap
    p2 = length $ filter (\(Cell x y) -> x == mw + 1 - (div mw 3) &&  y == mh + 1 - (div mh 3)) $ cellsm gameMap
    MapSize mw mh = sizem gameMap
    t = turnm gameMap
  
flipmap gm = map (\(Cell x y) -> Cell (mx + 1 - x) (my + 1 - y)) cs
  where
    cs = cellsm gm
    MapSize mx my = sizem gm

invertelm l i = foldr inv l i
  where
    inv c a = if elem c a
              then delete c a
              else c:a

-- Game Step
stepGame :: Map -> Map 
stepGame m = m {cellsm = filter inrange $ stepCells (cellsm m), turnm = t + 1} 
  where
    inrange (Cell x y) = x > 0 && y > 0 && x <= mx && y <= my
    MapSize mx my = sizem m
    t = turnm m
    

stepCells :: [Cell] -> [Cell]
stepCells m = nub $ foldr (\c a -> a ++ (allLive m c)) [] m
  where
    allLive m c = (nei m c) ++ (cell m c)
    nei m c = filter (isAlive m False) (getEmptyNeighbours m c)
    cell m c = filter (isAlive m True) [c]

isAlive :: [Cell] -> Bool -> Cell -> Bool
isAlive m True c = elem (numNeighbour m c) [2,3]
isAlive m False c = elem (numNeighbour m c) [3]

getEmptyNeighbours :: [Cell] -> Cell -> [Cell]
getEmptyNeighbours m (Cell tx ty) = filter (\x -> not $ elem x m) [Cell x y | x <-[tx-1..tx+1], y <- [ty-1..ty+1], x/=tx && y/=ty]

numNeighbour :: [Cell] -> Cell -> Int
numNeighbour m t = length $ filter (isNeighbour t) m

isNeighbour :: Cell -> Cell -> Bool
isNeighbour a b 
  | abs (xc a - xc b) == 1
    && abs (yc a - yc b) < 2 = True
  | abs (yc a - yc b) == 1
    && abs (xc a - xc b) < 2 = True
  | otherwise = False


-- Temporary Showing
instance Show Map where
  show (Map m (MapSize x y) t) = (show t) ++ (unlines [[if (elem (Cell lx ly) m) then 'x' else '_' |lx <-[1..x]] | ly <- [1..y]])

