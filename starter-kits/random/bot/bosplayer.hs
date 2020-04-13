import System.IO
import Data.List


main = do
 raw <- getLine
 if "Initialize" == (head $ words raw)
 then initialize raw
 else takeTurn raw

initialize raw = do
  let parsed = map read $ words raw
  let me = parsed!!1
  let startingLoc = (parsed!!2, parsed!!3)
  print $ Memory me startingLoc
  hFlush stdout

takeTurn raw = do
  let mem = read raw
  putStrLn $ (show $ fst $ loc mem) ++ " " ++ (show $ snd $ loc mem) ++ " " ++ show 100
  hFlush stdout
  map_ <- parseMap <$> getLine
  let mycells = filter (isMine (me mem)) map_
  let commands = concat $ map gnb mycells
  let commandString = parseCommandString commands
  putStrLn $ "" ++ commandString
  print mem
  hFlush stdout


data Memory = Memory { me :: Int
                     , loc :: (Int, Int)
                     } deriving (Read, Show)


parseMap :: String -> [(Int, Int, Int)]
parseMap raw = threeify $ map read $ take 30 $ words raw
  where
    threeify (a:b:c:rst) = (a,b,c):(threeify rst)
    threeify [] = []


isMine :: Int -> (Int, Int, Int) -> Bool
isMine me (_, _, i) = i == me

gnb (a,b,c) = [(a+i, b+j, c)| j <- [-1,1], i <- [0,1]]


parseCommandString commands = intercalate " " $ map show $ concat $ map (\(a, b, _)->[a,b]) commands

