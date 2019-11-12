import System.IO
import Data.List

main = turn

parseMap :: String -> [(Int, Int, Int)]
parseMap raw = threeify $ map read $ words raw
  where
    threeify (a:b:c:rst) = (a,b,c):(threeify rst)
    threeify [] = []


isMine :: (Int, Int, Int) -> Bool
isMine (_, _, i) = i == 2

gnb (a,b,c) = [(a+i, b+j, c)| j <- [-1,1], i <- [0,1]]


parseCommandString commands = intercalate " " $ map show $ concat $ map (\(a, b, _)->[a,b]) commands

turn :: IO ()
turn = do
  raw <- getLine
  let cells = parseMap raw
  let mycells = filter isMine cells
  let commands = concat $ map gnb mycells
  let commandString = parseCommandString commands
  putStrLn $ "" ++ commandString
  turn
