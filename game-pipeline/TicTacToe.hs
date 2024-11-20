module Main where

import Data.List (intercalate)
import Data.Int (Int8)
import qualified Data.Vector as V
import System.IO
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

-- Types

data Player = X | O deriving (Eq, Show, Generic, FromJSON, ToJSON)

type Board = [[Maybe Player]]

data PlayerResponse = PlayerResponse Int Int deriving (Show)

data GameEngineInMessage = GameStart Int | PlayerResponses [(Int, Bool, PlayerResponse)] deriving (Show)

data GameEngineOutMessage = GameEnd [(Int, Float)] | PlayerTurn [(Int, Board)] | DebugMessage String deriving (Show)

-- JSON Parsers

instance FromJSON PlayerResponse where
  parseJSON = withObject "PlayerResponse" $ \obj -> do
    [x, y] <- obj .: "move"
    if inRange x y
      then
        return (PlayerResponse x y)
      else
        fail "Coordinates out of bounds"
    where
      inRange :: Int -> Int -> Bool
      inRange x y = x >= 0 && x <= 2 && y >= 0 && y <= 2

instance FromJSON GameEngineInMessage where
  parseJSON = withObject "GameEngineInMessage" $ \obj -> do
    messageType <- (obj .: "messageType") :: (Parser String)
    case messageType of
      "gameStart" -> do
        numPlayers <- obj .: "numberOfPlayers" 
        return (GameStart numPlayers)
      "playerResponses" -> do
        responses <- obj .: "responses"
        parsed_responses <- mapM parse_response responses
        return (PlayerResponses parsed_responses)
    where
      parse_response = withObject "Player Response" $ \obj -> do
        player <- obj .: "player"
        response <- obj .: "response"
        isValid <- obj .: "isValid"
        return (player, isValid, response)

instance ToJSON GameEngineOutMessage where
  toJSON (DebugMessage message) = object [
    "messageType" .= ("debugMessage" :: String),
    "body" .= message
    ]
  toJSON (GameEnd scores) = object [
    "messageType" .= ("gameEnd":: String),
    "scores" .= map (\(player, score) -> object ["player" .= player, "score" .= score]) scores
    ]
  toJSON (PlayerTurn turns) = object [
    "messageType" .= ("playerTurn":: String),
    "scores" .= map (\(player, input) -> object ["player" .= player, "input" .= input]) turns
    ]

-- Initial empty board
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

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

checkGameStart :: GameEngineInMessage -> Either String Int
checkGameStart (GameStart i) = Right i
checkGameStart _ = Left "Expected game start message"

checkPlayerResponse :: GameEngineInMessage -> Either String PlayerResponse
checkPlayerResponse (PlayerResponses [(_, True, pr)]) = Right pr
checkPlayerResponse _ = Left "Expected player responses"

-- Check if the move is valid (within bounds and cell is empty)
isValidMove :: Board -> PlayerResponse -> Either String Bool
isValidMove board (PlayerResponse row col) = do
    if row >= 0 && row < 3 && col >= 0 && col < 3
      then
        Right True
      else
        Left "Invalid Move"
    case board !! row !! col of
        Nothing -> Right True
        _       -> Left "Invalid move"

-- Update the board with the player's move
updateBoard :: Board -> Player -> PlayerResponse -> Board
updateBoard board player (PlayerResponse row col) =
    take row board ++
    [take col (board !! row) ++ [Just player] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Determine the next player
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

flushedPutStrLn line = do
  putStrLn line
  hFlush stdout
flushedPutStrLnB line = do
  B.putStrLn (LB.toStrict line)
  hFlush stdout

intToPlayer 0 = X
intToPlayer 1 = O

playerToInt X = 0
playerToInt O = 1

otherPlayer X = O
otherPlayer O = X

applyMove :: Board -> Player -> PlayerResponse -> Either String Board
applyMove board player playerResponse = do
    isValidMove board playerResponse
    return $ updateBoard board player playerResponse

-- Main game loop
playGame :: Board -> Player -> IO ()
playGame board player
  | checkWin X board = flushedPutStrLnB $ encode $ GameEnd [(playerToInt X, 1), (playerToInt O, 0)]
  | checkWin O board = flushedPutStrLnB $ encode $ GameEnd [(playerToInt O, 1), (playerToInt X, 0)]
  | otherwise = do
    flushedPutStrLnB $ encode $ PlayerTurn [(playerToInt player, board)]
    imsg <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
    pr <- liftEither (checkPlayerResponse imsg)
    newboard <- liftEither (applyMove board player pr)
    playGame newboard (otherPlayer player)

-- Start the game
main :: IO ()
main = do
  message <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.getLine
  liftEither (checkGameStart message)
  playGame initialBoard X

