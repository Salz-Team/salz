{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module HandleBotCmds ( getBotCmds
                     , filterCmds
                     , applyCmds ) where

import qualified Database.PostgreSQL.Simple as DB
import qualified Network.AMQP as RMQ
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Step (getAround, dist, thrd)
import Database

data BotCmd = BotCmd
  { botId :: Int
  , botCmds :: [(Int, Int)]
  } deriving (Generic, Show)

instance ToJSON BotCmd
instance FromJSON BotCmd

-- msg data is a botcmd
-- deliveryHandler :: (Message, Envelope) -> IO ()
-- deliveryHandler (msg, metadata) = print $ T.unpack <$> botId <$> decode (msgBody msg)
-- get all messages that you can using getMsg

--------------------------------------------------------------------------------
-- getBotCmds
--------------------------------------------------------------------------------
getBotCmds :: RMQ.Channel -> T.Text -> IO [BotCmd]
getBotCmds conn rmqCmdChannelName = do
  readAllMsgs conn rmqCmdChannelName []


readAllMsgs :: RMQ.Channel -> T.Text -> [BotCmd] -> IO [BotCmd]
readAllMsgs conn rmqCmdChannelName cmds = do
  msg <- RMQ.getMsg conn RMQ.NoAck rmqCmdChannelName
  let mCmd = RMQ.msgBody <$> fst <$> msg >>= decode
  maybe (return cmds) (\cmd -> readAllMsgs conn rmqCmdChannelName (cmd:cmds)) mCmd


--------------------------------------------------------------------------------
-- filterCmds
--------------------------------------------------------------------------------
filterCmds :: DB.Connection -> [BotCmd] -> IO [BotCmd]
filterCmds conn cmds = do
  turn <- getMostRecentTurn conn
  lastFrame <- getFrame conn turn
  return $ filterCmds' lastFrame cmds

filterCmds' :: [(Int, Int, Int)] -> [BotCmd] -> [BotCmd]
filterCmds' frame cmds = map getHealthy cmds
  where
    getHealthy (BotCmd bid clst) = BotCmd bid (filter (isUnhealthy bid) clst)
    isUnhealthy bid (x, y) = 1 >= (length $ filter ((== bid). thrd) $ getAround frame 1 (x,y,bid))

--------------------------------------------------------------------------------
-- applyCmds
--------------------------------------------------------------------------------
applyCmds :: DB.Connection -> [BotCmd] -> IO ()
applyCmds conn botCmds = do
  turn <- getMostRecentTurn conn
  putFrame conn (turn + 1) cmds
  where
    format (BotCmd bid clst) = map (\(x,y) -> (x,y,bid)) clst
    cmds = concat $ map format botCmds

