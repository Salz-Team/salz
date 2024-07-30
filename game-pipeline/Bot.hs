module Main where

import Data.List
import Data.Maybe
import Control.Monad
import System.IO

-- Define the player markers
data Player = X | O deriving (Eq, Show)

-- Define the game board as a list of Maybe Player (X or O or empty)
type Board = [[Maybe Player]]

parseCharacter :: Char -> Maybe Player
parseCharacter 'X' = Just X
parseCharacter 'O' = Just O
parseCharacter ' ' = Nothing

-- Parse tic tac toe board
parseBoard :: [String] -> Board
parseBoard = map (map parseCharacter . removeEveryOther)
  where
    removeEveryOther :: [Char] -> [Char]
    removeEveryOther = map snd . filter (even . fst) . zip [0..]

-- Check if the board is full (tie game)
isFull :: Board -> Bool
isFull = all (all isJust)
  where
    isJust :: Maybe Player -> Bool
    isJust (Just _) = True
    isJust Nothing  = False

-- get Coords of board
boardCoords :: Board -> [(Maybe Player, (Int, Int))]
boardCoords = map (\(j, (i, x)) -> (x, (div (j - i) 3, i))) . zip [0..] . concat . map (zip [0..])

-- find first open spot
getOpenSpot :: Board -> (Int, Int)
getOpenSpot = snd . fromJust . find (isNothing . fst) . boardCoords

flushedPutStrLn line = do
  putStrLn line
  hFlush stdout

gameLoop :: IO ()
gameLoop = do
    input <- replicateM 3 getLine
    let board = parseBoard input
    let move = getOpenSpot board
    flushedPutStrLn $ show 1
    flushedPutStrLn $ (show (fst move)) ++ " " ++ (show (snd move))
    gameLoop


main :: IO ()
main = gameLoop


