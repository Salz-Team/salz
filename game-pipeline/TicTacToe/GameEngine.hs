module Main where

import System.IO
import Data.Aeson
import Types
import TicTacToe.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Utils

-- Initial empty board
initialBoard :: TicTacToeBoard
initialBoard = replicate 3 (replicate 3 Nothing)

-- Check if a player has won
checkWin :: TicTacToePlayer -> TicTacToeBoard -> Bool
checkWin player board =
    any checkRow board ||        -- Check rows
    any checkRow (columns board) ||  -- Check columns
    checkDiagonals board         -- Check diagonals
  where
    checkRow :: [Maybe TicTacToePlayer] -> Bool
    checkRow row = all (== Just player) row

    columns :: TicTacToeBoard -> TicTacToeBoard
    columns = foldr (zipWith (:)) (repeat [])

    checkDiagonals :: TicTacToeBoard -> Bool
    checkDiagonals bd = all (== Just player) (diag1 bd) || all (== Just player) (diag2 bd)

    diag1 :: TicTacToeBoard -> [Maybe TicTacToePlayer]
    diag1 bd = [bd !! i !! i | i <- [0..length bd - 1]]

    diag2 :: TicTacToeBoard -> [Maybe TicTacToePlayer]
    diag2 bd = [bd !! i !! (length bd - 1 - i) | i <- [0..length bd - 1]]

-- Check if the board is full (tie game)
isFull :: TicTacToeBoard -> Bool
isFull = all (all isJust)
  where
    isJust :: Maybe TicTacToePlayer -> Bool
    isJust (Just _) = True
    isJust Nothing  = False

checkGameStart :: GameEngineInMessage TicTacToePlayerResponse -> Either String Int
checkGameStart (GameStart i) = Right i
checkGameStart _ = Left "Expected game start message"

checkPlayerResponse :: GameEngineInMessage TicTacToePlayerResponse -> Either String TicTacToePlayerResponse
checkPlayerResponse (PlayerResponses [(_, True, pr)]) = Right pr
checkPlayerResponse _ = Left "Expected player responses"

-- Check if the move is valid (within bounds and cell is empty)
isValidMove :: TicTacToeBoard -> TicTacToePlayerResponse -> Either String Bool
isValidMove board (TicTacToePlayerResponse col row) = do
    _ <- if row >= 0 && row < 3 && col >= 0 && col < 3
      then
        Right True
      else
        Left "Invalid Move"
    case board !! row !! col of
        Nothing -> Right True
        _       -> Left "Invalid move"

-- Update the board with the player's move
updateBoard :: TicTacToeBoard -> TicTacToePlayer -> TicTacToePlayerResponse -> TicTacToeBoard
updateBoard board player (TicTacToePlayerResponse col row) =
    take row board ++
    [take col (board !! row) ++ [Just player] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Determine the next player
nextPlayer :: TicTacToePlayer -> TicTacToePlayer
nextPlayer X = O
nextPlayer O = X

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

applyMove :: TicTacToeBoard -> TicTacToePlayer -> TicTacToePlayerResponse -> Either String TicTacToeBoard
applyMove board player playerResponse = do
    _ <- isValidMove board playerResponse
    return $ updateBoard board player playerResponse

-- Main game loop
playGame :: TicTacToeBoard -> TicTacToePlayer -> IO ()
playGame board player
  | checkWin X board = flushedPutStrLnB stdout $ encode $ (GameEnd [(playerToInt X, 1), (playerToInt O, 0)] :: GameEngineOutMessage TicTacToeBoard)
  | checkWin O board = flushedPutStrLnB stdout $ encode $ (GameEnd [(playerToInt O, 1), (playerToInt X, 0)] :: GameEngineOutMessage TicTacToeBoard)
  | otherwise = do
    flushedPutStrLnB stdout $ encode $ PlayerTurn [(playerToInt player, board)]
    imsg <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
    pr <- liftEither (checkPlayerResponse imsg)
    newboard <- liftEither (applyMove board player pr)
    playGame newboard (otherPlayer player)
    where
      otherPlayer X = O
      otherPlayer O = X
      playerToInt X = 0
      playerToInt O = 1

-- Start the game
main :: IO ()
main = do
  flushedPutStrLnB stdout $ encode $ (GameOStart "tic-tac-toe" :: GameEngineOutMessage TicTacToeBoard)
  message <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
  _ <- liftEither (checkGameStart message)
  playGame initialBoard X

