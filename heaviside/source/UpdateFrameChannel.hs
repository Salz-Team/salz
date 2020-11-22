{-# LANGUAGE OverloadedStrings #-}
module UpdateFrameChannel ( updateFrameChannel ) where


import qualified Database.PostgreSQL.Simple as DB
import qualified Network.AMQP  as RMQ
import qualified Data.Text as T
import Data.Aeson

import Database

updateFrameChannel :: DB.Connection -> RMQ.Channel -> T.Text -> IO ()
updateFrameChannel conn chan rmqFrameChannelName = do
  turn <- getMostRecentTurn conn
  lastFrame <- getFrame conn turn
  RMQ.publishMsg chan "" rmqFrameChannelName
            (RMQ.newMsg {RMQ.msgBody         = encode lastFrame,
                     RMQ.msgDeliveryMode = Just RMQ.NonPersistent})
  return ()

