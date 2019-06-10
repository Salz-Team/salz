import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.Process
import System.IO
import GHC.IO.Handle
import Text.Read


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

main :: IO ()
main = getArgs >>= handleArgs >>= setup >>= gameTurn


-- Handle Arguments
data Options = Options
  { oMapWidth :: Int
  , oMapHeight :: Int
  , oPlayer1Path :: String
  , oPlayer2Path :: String
  } deriving (Eq)

handleArgs :: [String] -> IO (Maybe Options)
handleArgs [a, b, c, d] =
  case readOptions [a, b, c, d] of
  Nothing -> usage >> pure Nothing
  Just o -> pure (Just o)

handleArgs ["-h"] = usage >> pure Nothing
handleArgs _ = usage >> pure Nothing

maybeRead = fmap fst . listToMaybe . reads

usage :: IO ()
usage = hPutStrLn stderr "Usage:\ngame w h play1 play2"

readOptions :: [String] -> Maybe Options
readOptions [a, b, c, d] = do
  w <- maybeRead a
  h <- maybeRead a
  return (Options w h c d)

-- Setup Players

type Orientation = Bool

data Player = Player 
  { pstdin :: Handle
  , pstdout :: Handle
  , pname :: String
  }

data GameState = GameState
  { gsplayer1 :: Player
  , gsplayer2 :: Player
  , gsmap :: Map
  }

setupPlayer :: Map -> String -> IO Player
setupPlayer map path = do
  (stdin, stdout, _, _) <- runInteractiveCommand path
  --Send Map
  let (MapSize mw mh) = sizem map
  hPutStr stdin ((show mw) ++ " " ++ (show mh))
  name <- hGetLine stdout
  return $ Player
    { pstdin = stdin
    , pstdout = stdin
    , pname = name
    }
  

setup:: Maybe Options -> IO (Maybe GameState)
setup Nothing = pure Nothing
setup (Just options) = do
  let gm = Map [] (MapSize (oMapWidth options) (oMapHeight options)) 0
  player1 <- setupPlayer gm (oPlayer1Path options)
  player2 <- setupPlayer gm (oPlayer2Path options)
  putStrLn ((pname player1) ++ " " ++ (pname player2))
  return $ Just $ GameState
    { gsplayer1 = player1
    , gsplayer2 = player2
    , gsmap = gm
    }
  
gameTurn Nothing = pure ()
gameTurn (Just gs) = do
  let stdout1 = pstdout $ gsplayer1 gs
  let stdout2 = pstdout $ gsplayer2 gs
  let stdin1 = pstdin $ gsplayer1 gs
  let stdin2 = pstdin $ gsplayer2 gs
  let gamemap = gsmap gs

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
  then gameTurn $ Just $ gs {gsmap = ngamemap}
  else print $ isWin ngamemap

isWin gameMap 
  | p1 > 0 && p2 > 0 = 3
  | p1 > 0 = 1
  | p2 > 0 = 2
  | t >= 1000 = 3
  | otherwise = 0
  where
    p1 = length $ filter (\(Cell x y) -> x == (div mw 3) &&  y == (div mh 3)) $ cellsm gameMap
    p2 = length $ filter (\(Cell x y) -> x == mw - (div mw 3) &&  y == mh - (div mh 3)) $ cellsm gameMap
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

