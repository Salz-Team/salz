{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module HandleBotCmds ( getBotCmds
                     , filterCmds
                     , applyCmds ) where

import qualified Database.PostgreSQL.Simple as DB
import qualified Network.AMQP as RMQ
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data BotCmd = BotCmd
  { botId :: T.Text
  , botCmds :: [(Int, Int)]
  } deriving (Generic, Show)

instance ToJSON BotCmd
instance FromJSON BotCmd

-- msg data is a botcmd
-- deliveryHandler :: (Message, Envelope) -> IO ()
-- deliveryHandler (msg, metadata) = print $ T.unpack <$> botId <$> decode (msgBody msg)
--- get all messages that you can using getMsg

getBotCmds :: RMQ.Channel -> IO [BotCmd]
getBotCmds conn = return []

filterCmds :: DB.Connection -> [BotCmd] -> IO [BotCmd]
filterCmds conn _ = return []

applyCmds :: DB.Connection -> [BotCmd] -> IO ()
applyCmds conn _ = return ()

