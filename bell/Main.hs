{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Network.AMQP
import System.Environment

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Data.Aeson
import GHC.Generics

envVariableNames = ["RMQUSER" ,"RMQPWD" ,"RMQPORT" , "RMQHOST"]
rmqChannelName = "botcmds"

data BotCmd = BotCmd
  { botId :: T.Text
  , botCmds :: [(Int, Int)]
  } deriving (Generic, Show)

instance ToJSON BotCmd
instance FromJSON BotCmd

main :: IO ()
main = do
  envVars <- mapM getEnv envVariableNames

  conn <- openConnection (envVars!!3) "/" (T.pack $ envVars!!0) (T.pack $ envVars!!1)
  ch   <- openChannel conn

  declareQueue ch newQueue {queueName       = rmqChannelName,
                           queueAutoDelete = False,
                           queueDurable    = False}

  publishMsg ch "" rmqChannelName
            (newMsg {msgBody         = encode (BotCmd "1" [(1,1), (2,2), (3,3)]),
                     msgDeliveryMode = Just NonPersistent})

  BL.putStrLn " [x] Sent 'Hello World!'"
  closeConnection conn
