module Parsers where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics

-- Types

data GameEngineInMessage x = GameStart Int | PlayerResponses [(Int, Bool, x)] deriving (Show)

data GameEngineOutMessage x = GameEnd [(Int, Float)] | PlayerTurn [(Int, x)] | DebugMessage String deriving (Show)

-- JSON Parsers

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
    "scores" .= map (\(player, input) -> object ["player" .= player, "input" .= input]) turns
    ]

