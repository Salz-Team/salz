{-# LANGUAGE OverloadedStrings #-}
module ExternalProcessHandler
  ( ExternalProcessHandler
  , Error
  , createExternalProcess
  , timedCallandResponse
  , cleanPlayer
  )
    where

import Control.Monad()
import Control.Monad.Trans.Except
import qualified Control.Exception as CE
import qualified System.FilePath as FP
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

createExternalProcess :: FilePath -> IO (Either T.Text ExternalProcessHandler)
createExternalProcess path = runExceptT $ do
  (psin, psout, pserr, p) <- withExceptT transform $ ExceptT $ CE.try $ startProcess path
  return $ ExternalProcessHandler psin psout pserr p
  where
    startProcess path1 = runInteractiveProcess "bash" [path1] (Just $ FP.dropFileName path1) Nothing

cleanPlayer :: ExternalProcessHandler -> IO ()
cleanPlayer eh = do
  cleanupProcess (Just $ pstdin eh, Just $ pstdout eh, Just $ pstderr eh, pproc eh)

transform :: IOError -> T.Text
transform _ = "Player bot crashed"

timedCallandResponse :: Int -> ExternalProcessHandler -> T.Text -> IO (Either T.Text T.Text)
timedCallandResponse time eph call = runExceptT $ do
  --give map
  withExceptT transform $ ExceptT $ CE.try $ hPutStr (pstdin eph) $ (T.unpack call) ++ "\n"
  withExceptT transform $ ExceptT $ CE.try $ hFlush (pstdin eph)

  TM.lift $ putStrLn $ "Call is:" ++ (T.unpack call)

  -- check stdout
  check "EndOfFile" $ hIsEOF (pstdout eph)
  TM.lift $ putStrLn "Done checks"

  response <- ExceptT $ eitherTimeout time "OutOfTime" $ hGetLine (pstdout eph)

  return $ T.pack response

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
