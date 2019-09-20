{-# LANGUAGE OverloadedStrings #-}
module ExternalProcessHandler
  ( ExternalProcessHandler
  , Error
  , createExternalProcess
  , timedCallandResponse
  , replaceExecutable
  , cleanPlayer
  )
    where

import Control.Monad()
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Class as TM
import qualified Data.Text as T
import Data.Either()

import System.Process
import System.Timeout
import GHC.IO.Handle


data ExternalProcessHandler = ExternalProcessHandler
  { pstdin :: Handle
  , pstdout :: Handle
  , pstderr :: Handle
  , pproc :: ProcessHandle
  }

data Error = OutOfTime | EndOfFile | StdoutClosed
  deriving (Show)

createExternalProcess :: FilePath -> IO ExternalProcessHandler
createExternalProcess path = do
  (psin, psout, pserr, p) <- runInteractiveCommand path
  return $ ExternalProcessHandler psin psout pserr p

cleanPlayer :: ExternalProcessHandler -> IO ()
cleanPlayer eh = do
  cleanupProcess (Just $ pstdin eh, Just $ pstdout eh, Just $ pstderr eh, pproc eh)

timedCallandResponse :: Int -> ExternalProcessHandler -> T.Text -> IO (Either T.Text T.Text)
timedCallandResponse time eph call = runExceptT $ do
  --give map
  TM.lift $ hPutStr (pstdin eph) $ (T.unpack call) ++ "\n"
  TM.lift $ hFlush (pstdin eph)

  -- check stdout
  check "EndOfFile" $ hIsEOF (pstdout eph)
  TM.lift $ putStrLn "Done checks"

  response <- ExceptT $ eitherTimeout time "OutOfTime" $ hGetLine (pstdout eph)

  return $ T.pack response

replaceExecutable :: FilePath -> ExternalProcessHandler -> IO ExternalProcessHandler
replaceExecutable path _ = createExternalProcess path

check :: T.Text -> IO Bool -> ExceptT T.Text IO ()
check er f = do
  open <- TM.lift f
  if open
  then ExceptT (return (Left er))
  else ExceptT (return (Right ()))

eitherTimeout :: Int -> T.Text -> IO a -> IO (Either T.Text a)
eitherTimeout time errorMsg action = do
  maybeResult <- timeout time action
  return $ fromMay maybeResult

  where
    fromMay Nothing  = Left errorMsg
    fromMay (Just r) = Right r
