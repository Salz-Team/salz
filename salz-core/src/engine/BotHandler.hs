{-# LANGUAGE OverloadedStrings #-}
module BotHandler
  ( BotHandler
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
import Types

data BotHandler =
    NewBot { filepath :: FilePath
           , playerId :: Int
           , startingLocation :: (Int, Int)
           }
    | Bot { filepath :: FilePath
          , playerId :: Int
          , memory :: T.Text
          , errorLog :: T.Text
          , commands :: [(Int, Int)]
          } deriving Show


data RunError = RunError T.Text
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

tryRunError :: IO a -> IO (Either T.Text a)
tryRunError f = mapLeft (\(RunError txt) -> txt) <$> CE.try f

initializeBot :: BotHandler -> IO (E.Either T.Text BotHandler)
initializeBot bot = tryRunError $ withProcessWait botConfig $ \process -> do
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

takeTurn :: Board w h CellInfo -> BotHandler -> IO (E.Either T.Text BotHandler)
takeTurn world bot = tryRunError $ withProcessWait botConfig $ \process -> do
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
  where
    mem = memory bot
    fp = filepath bot
    botConfig = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ setWorkingDir (dropFileName fp)
              $ fromString fp

