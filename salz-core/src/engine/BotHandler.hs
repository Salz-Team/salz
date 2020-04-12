{-# LANGUAGE OverloadedStrings #-}
module BotHandler
  ( Bot(..)
  , takeTurn
  )
    where

import qualified Data.Text as T
import qualified Data.Either as E
import Data.Either.Combinators
import qualified Control.Exception as CE
import Control.DeepSeq
import Data.String
import System.FilePath
import System.Process.Typed
import System.Timeout
import System.IO
import System.Exit
import GHC.IO.Handle

import qualified Map as Map

data Bot = Crashed { playerId :: Int
                   , errorLog :: T.Text
                   , memory :: T.Text
                   , errorMsg :: T.Text
                   }
          | NewBot { playerId :: Int
                   , filePath :: FilePath
                   , startingLocation :: (Int, Int)
                   }
          | Bot    { playerId :: Int
                   , filePath :: FilePath
                   , memory :: T.Text
                   , errorLog :: T.Text
                   , commands :: [(Int, Int)]
                   } deriving Show



data RunError = RunError Bot
  deriving Show

instance CE.Exception RunError

tShow :: Show a => a -> T.Text
tShow = T.pack . show

fromJustException :: Maybe a -> T.Text -> IO a
fromJustException (Just a) _ = return a
fromJustException Nothing er = CE.throwIO (RunError er)

timedCallandResponse :: Int -> Handle -> Handle -> T.Text -> IO T.Text
timedCallandResponse time stdin stdout call = do
  hPutStrLn stdin (T.unpack call)
  hFlush stdin

  response <- timeout time (T.pack <$> (hGetLine stdout))
  fromJustException response ("Time ran out on call: " `T.append` call)

timedWaitExitCode :: Int -> Process Handle Handle Handle -> IO ExitCode
timedWaitExitCode time process = do
  exitCode <- timeout time (waitExitCode process)
  fromJustException exitCode "Time ran out waiting for bot to shutdown"

tryRunError :: IO Bot -> IO Bot
tryRunError b = E.either (\(RunError bot) -> bot) id <$> CE.try b

initializeBot :: Bot -> IO Bot
initializeBot (Crashed a b c d) = return (Crashed a b c d)
initializeBot (Bot a b c d e) = return (Bot a b c d e)
initializeBot (NewBot pid fp sloc) = withProcessWait botConfig $ \process -> do
    let call = T.intercalate " " ["Initialize", tShow pid, tShow (fst sloc), tShow (snd sloc)]
    memory <- timedCallandResponse 100 (getStdin process) (getStdout process) call
    hClose (getStdin process)


    exitCode <- timedWaitExitCode 100 process

    errormsg' <- T.pack <$> hGetContents (getStderr process)
    errormsg <- CE.evaluate $ force errormsg'

    return $ Bot { filepath = fp
                 , playerId = pid
                 , memory = memory
                 , errorLog = errormsg
                 , commands = []
                 }
  where
    fp = filepath bot
    sloc = startingLocation bot
    pid = playerId bot
    botConfig = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ setWorkingDir (dropFileName fp)
              $ fromString fp

takeTurn :: Map.Map -> BotHandler -> IO Bot
takeTurn world (NewBot fp pid sloc) = initializeBot (NewBot fp pid sloc) >>= takeTurn world
takeTurn world (Bot fp _ mem _ _ ) = tryRunError $ withProcessWait botConfig $ \process -> do
    mapReq <- timedCallandResponse 100 (getStdin process) (getStdout process) mem

    let reqMap = undefined
    rawCmds <- timedCallandResponse 100 (getStdin process) (getStdout process) reqMap
    newMem <- timedCallandResponse 100 (getStdin process) (getStdout process) ""
    hClose (getStdin process)

    exitCode <- timedWaitExitCode 100 process

    errormsg' <- T.pack <$> hGetContents (getStderr process)
    errormsg <- CE.evaluate $ force errormsg'

    return $ bot { errorLog = errormsg
                 , commands = undefined -- rawCmds
                 , memory = newMem
                 }
takeTurn _ bot = return bot
  where
    botConfig = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ setWorkingDir (dropFileName fp)
              $ fromString fp

