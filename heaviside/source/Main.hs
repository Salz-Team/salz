{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import System.Environment
import Control.Concurrent

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BN

import qualified Network.AMQP as RMQ

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as DB

import Step (step)
import HandleBotCmds (getBotCmds, filterCmds, applyCmds)
import UpdateFrameChannel  (updateFrameChannel)

envVariableNames = ["RMQUSER" ,"RMQPWD" ,"RMQPORT" ,"PSQLUSER" ,"PSQLPWD" ,"PSQLDB" ,"PSQLPORT"]
rmqCmdChannelName = "botcmds"
rmqFrameChannelName = "frames"



main :: IO ()
main = do
  envVars <- mapM getEnv envVariableNames

  dbConn <- DB.connectPostgreSQL $ BN.pack $ "host=localhost port="
                             ++ (envVars!!6)
                             ++ " dbname="
                             ++ (envVars!!5)
                             ++ " password="
                             ++ (envVars!!4)
                             ++ " user="
                             ++ (envVars!!3)

  rmqConn <- RMQ.openConnection "127.0.0.1" "/" (T.pack $ envVars!!0) (T.pack $ envVars!!1)
  rmqCmdChan   <- RMQ.openChannel rmqConn
  rmqFrameChan   <- RMQ.openChannel rmqConn

  RMQ.declareQueue rmqCmdChan RMQ.newQueue {RMQ.queueName       = rmqCmdChannelName,
                                     RMQ.queueAutoDelete = False,
                                     RMQ.queueDurable    = False}
  RMQ.declareQueue rmqFrameChan RMQ.newQueue {RMQ.queueName       = rmqFrameChannelName,
                                     RMQ.queueAutoDelete = False,
                                     RMQ.queueDurable    = False}

  gameLoop rmqCmdChan rmqFrameChan dbConn

  RMQ.closeConnection rmqConn
  DB.close dbConn

gameLoop :: RMQ.Channel -> RMQ.Channel -> DB.Connection -> IO ()
gameLoop rmqCmdChan rmqFrameChan dbConn = do
  -- get, filter, then apply bot cmds
  cmds <- getBotCmds rmqCmdChan rmqCmdChannelName
  filteredCmds <- filterCmds dbConn cmds
  applyCmds dbConn filteredCmds

  -- step the game
  step dbConn

  -- send new frame to bots
  updateFrameChannel dbConn rmqFrameChan rmqFrameChannelName

  -- wait 2 seconds and then go to next turn
  threadDelay 2000000
  gameLoop rmqCmdChan rmqFrameChan dbConn
