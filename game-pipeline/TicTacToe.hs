module Main where

import Data.List (intercalate)
import Data.Int (Int8)
import qualified Data.Vector as V
import System.IO
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics

-- Define the player markers
data Player = X | O deriving (Eq, Show, Generic, FromJSON, ToJSON)

playerToInt X = 0
playerToInt O = 1

intToPlayer 0 = X
intToPlayer 1 = O

flushedPutStrLn line = do
  putStrLn line
  hFlush stdout

type Board = [[Maybe Player]]

data PlayerResponse = PlayerResponse Int8 Int8 deriving (Show)

instance FromJSON PlayerResponse where
  parseJSON = withObject "PlayerResponse" $ \obj -> do
    [x, y] <- obj .: "move"
    if inRange x y
      then
        return (PlayerResponse x y)
      else
        fail "Coordinates out of bounds"
    where
      inRange :: Int8 -> Int8 -> Bool
      inRange x y = x >= 0 && x <= 2 && y >= 0 && y <= 2

data GameEngineInMessage = GameStart Int | PlayerResponses [(Int, Bool, PlayerResponse)] deriving (Show)

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

data GameEngineOutMessage = GameEnd [(Int, Float)] | PlayerTurn [(Int, Board)] | DebugMessage String deriving (Show)

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

-- Main game loop
playGame :: Board -> GameEngineInMessage -> IO ()
playGame board (GameStart player) = do
    flushedPutStrLn $ encode $ PlayerTurn [(player, board)]
    imsg <- liftEither =<< eitherDecode =<< getLine
    (newBoard, omsg) <- =<< (playGame_ board imsg)
    playGame board imsg

playGame board imsg = do
    (newBoard, omsg) <- (playGame_ board imsg) >>= fromEither
    flushedPutStrLn $ encode $ omsg
    newImsg <- eitherDecode >>= getLine
    playGame newBoard newImsg

playGame_ :: Board -> GameEngineInMessage -> Either String (Board, GameEngineOutMessage)
playGame_ board msg_ = do
    msg <- msg_
    (player, playerResponse) <- case msg of
      (PlayerResponses [(player, True, response)]) -> Right (intToPlayer player, response)
      _ -> Left "Expected one valid player response"

    isValidMove board playerResponse
    let newBoard = updateBoard board player playerResponse
    let otherPlayer = nextPlayer player
    if checkWin player newBoard
      then
        Right $ GameEnd [(playerToInt player, 1), (playerToInt otherPlayer, 0)]
      else
        Right $ PlayerTurn [(otherPlayer, newBoard)]

-- Check if the move is valid (within bounds and cell is empty)
isValidMove :: Board -> PlayerResponse -> Either String Bool
isValidMove board (PlayerResponse row col) =
    row >= 0 && row < 3 &&
    col >= 0 && col < 3 &&
    case board !! row !! col of
        Nothing -> Right True
        _       -> Left "Invalid move"
isValidMove _ _ = Left "Expecting a player response"

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


-- Start the game
main :: IO ()
main = do
  message <- liftEither =<< decodeEither <$> getLine
  playGame initialBoard message
