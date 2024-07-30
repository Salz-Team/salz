module Main where

import Data.List (intercalate)
import System.IO

-- Define the player markers
data Player = X | O deriving (Eq, Show)

playerToInt X = 0
playerToInt O = 1

flushedPutStrLn line = do
  putStrLn line
  hFlush stdout

-- Define the game board as a list of Maybe Player (X or O or empty)
type Board = [[Maybe Player]]

-- Initial empty board
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

-- Display the board
displayBoard :: Board -> IO ()
displayBoard board = do
    flushedPutStrLn $ intercalate "\n" $ map (intercalate " " . map (maybe " " showPlayer)) board
  where
    showPlayer :: Player -> String
    showPlayer X = "X"
    showPlayer O = "O"

-- Check if a player has won
checkWin :: Player -> Board -> Bool
checkWin player board =
    any checkRow board ||        -- Check rows
    any checkRow (columns board) ||  -- Check columns
    checkDiagonals board         -- Check diagonals
  where
    checkRow :: [Maybe Player] -> Bool
    checkRow row = all (== Just player) row

    columns :: Board -> Board
    columns = foldr (zipWith (:)) (repeat [])

    checkDiagonals :: Board -> Bool
    checkDiagonals bd = all (== Just player) (diag1 bd) || all (== Just player) (diag2 bd)

    diag1 :: Board -> [Maybe Player]
    diag1 bd = [bd !! i !! i | i <- [0..length bd - 1]]

    diag2 :: Board -> [Maybe Player]
    diag2 bd = [bd !! i !! (length bd - 1 - i) | i <- [0..length bd - 1]]

-- Check if the board is full (tie game)
isFull :: Board -> Bool
isFull = all (all isJust)
  where
    isJust :: Maybe Player -> Bool
    isJust (Just _) = True
    isJust Nothing  = False


-- Main game loop
playGame :: Board -> Player -> IO ()
playGame board player= do
    flushedPutStrLn $ "Player " ++ (show (playerToInt player)) ++ " " ++ (show 3)
    displayBoard board
    input <- getLine
    let [row, col] = map read $ words input
    if isValidMove board row col
        then do
            let newBoard = updateBoard board player row col
            if checkWin player newBoard
                then do
                    flushedPutStrLn $ "Finished " ++ (if playerToInt (player) == 0 then "1.0 -1.0" else "-1.0 1.0")
                else if isFull newBoard
                    then flushedPutStrLn "Done: It's a tie!"
                    else playGame newBoard (nextPlayer player)
        else do
            flushedPutStrLn "Error: Invalid move. Please try again."
            playGame board player

-- Check if the move is valid (within bounds and cell is empty)
isValidMove :: Board -> Int -> Int -> Bool
isValidMove board row col =
    row >= 0 && row < 3 &&
    col >= 0 && col < 3 &&
    case board !! row !! col of
        Nothing -> True
        _       -> False

-- Update the board with the player's move
updateBoard :: Board -> Player -> Int -> Int -> Board
updateBoard board player row col =
    take row board ++
    [take col (board !! row) ++ [Just player] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Determine the next player
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- Start the game
main :: IO ()
main = do
  numPlayers <- getLine
  playGame initialBoard X
