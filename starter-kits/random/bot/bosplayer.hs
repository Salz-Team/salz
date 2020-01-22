import System.IO
import Data.List

main = minit >>= turn

minit = do
  raw <- getLine
  putStrLn ""
  return $ read $ (words raw)!!0

parseMap :: String -> [(Int, Int, Int)]
parseMap raw = threeify $ map read $ take 30 $ words raw
  where
    threeify (a:b:c:rst) = (a,b,c):(threeify rst)
    threeify [] = []


isMine :: Int -> (Int, Int, Int) -> Bool
isMine me (_, _, i) = i == me

gnb (a,b,c) = [(a+i, b+j, c)| j <- [-1,1], i <- [0,1]]


parseCommandString commands = intercalate " " $ map show $ concat $ map (\(a, b, _)->[a,b]) commands

turn :: Int -> IO ()
turn me = do
  raw <- getLine
  let cells = parseMap raw
  let mycells = filter (isMine me ) cells
  let commands = concat $ map gnb mycells
  let commandString = parseCommandString commands
  putStrLn $ "" ++ commandString
  turn me
