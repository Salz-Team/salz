module Parsers where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics

-- Types

data GameEngineInMessage x = GameStart Int | PlayerResponses [(Int, Bool, x)] deriving (Show)
data GameEngineOutMessage x = GameEnd [(Int, Float)] | PlayerTurn [(Int, x)] | DebugMessage String deriving (Show)

data GameHistoryLine x = HGameStart Int String | HGameEnd [(Int, Float)] | HPlayerResponses [(Int, x, Bool, String, String)] | HDebug String

data TicTacToePlayerResponse = TicTacToePlayerResponse Int Int deriving (Show)
type TicTacToeBoard = [[Maybe TicTacToePlayer]]
data TicTacToePlayer = X | O deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- JSON Parsers

instance FromJSON TicTacToePlayerResponse where
  parseJSON = withObject "PlayerResponse" $ \obj -> do
    [x, y] <- obj .: "move"
    if inRange x y
      then
        return (TicTacToePlayerResponse x y)
      else
        fail "Coordinates out of bounds"
    where
      inRange :: Int -> Int -> Bool
      inRange x y = x >= 0 && x <= 2 && y >= 0 && y <= 2

instance ToJSON TicTacToePlayerResponse where
  toJSON (TicTacToePlayerResponse x y) = object [
    "move" .= [x, y]
    ]

instance ToJSON x => ToJSON (GameHistoryLine x) where
  toJSON (HGameStart numPlayers gameType) = object [
    "messageType" .= ("gameStart":: String),
    "numberOfPlayers" .= numPlayers,
    "gameType" .= gameType
    ]
  toJSON (HGameEnd scores) = object [
    "messageType" .= ("gameEnd":: String),
    "scores" .= map (\(player, score) -> object ["player" .= player, "score" .= score]) scores
    ]
  toJSON (HPlayerResponses turns) = object [
    "messageType" .= ("playerResponses":: String),
    "responses" .= map buildResponse turns
    ]
    where
      buildResponse (player, response, isValid, errorType, errorMessage) = object [
        "player" .= player,
        "response" .= response,
        "isValid" .= isValid,
        "errorType" .= errorType,
        "errorMessage" .= errorMessage
        ]
  toJSON (HDebug message) = object [
    "messageType" .= ("debugMessage" :: String),
    "body" .= message
    ]

instance FromJSON x => FromJSON (GameEngineInMessage x) where
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

instance ToJSON x => ToJSON (GameEngineInMessage x) where
  toJSON (GameStart np) = object [
    "messageType" .= ("gameStart":: String),
    "numberOfPlayers" .= np
    ]
  toJSON (PlayerResponses rps) = object [
    "messageType" .= ("playerResponses":: String),
    "responses" .= map convert_response rps
    ]
    where
      convert_response (player, isValid, response) = object [
        "player" .= player,
        "response" .= response,
        "isValid" .= isValid
        ]

instance FromJSON x => FromJSON (GameEngineOutMessage x) where
  parseJSON = withObject "GameEngineOutMessage" $ \obj -> do
    messageType <- (obj .: "messageType") :: (Parser String)
    case messageType of
      "debugMessage" -> do
        body <- obj .: "body"
        return (DebugMessage body)
      "gameEnd" -> do
        scores <- obj .: "scores"
        parsed_scores <- mapM parse_score scores
        return (GameEnd parsed_scores)
      "playerTurn" -> do
        inputs <- obj .: "inputs"
        parsed_inputs <- mapM parse_input inputs
        return (PlayerTurn parsed_inputs)
    where
      parse_score = withObject "Score" $ \obj -> do
        player <- obj .: "player"
        score <- obj .: "score"
        return (player, score)
      parse_input = withObject "Input" $ \obj -> do
        player <- obj .: "player"
        input <- obj .: "input"
        return (player, input)

instance ToJSON x => ToJSON (GameEngineOutMessage x) where
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
    "inputs" .= map (\(player, input) -> object ["player" .= player, "input" .= input]) turns
    ]

