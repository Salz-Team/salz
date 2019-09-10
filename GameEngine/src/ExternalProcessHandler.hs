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

createExternalProcess :: T.Text -> IO ExternalProcessHandler
createExternalProcess path = do
  (psin, psout, pserr, p) <- runInteractiveCommand $ T.unpack path
  return $ ExternalProcessHandler psin psout pserr p

cleanPlayer :: ExternalProcessHandler -> IO ()
cleanPlayer eh = do
  cleanupProcess (Just $ pstdin eh, Just $ pstdout eh, Just $ pstderr eh, pproc eh)

timedCallandResponse :: Int -> ExternalProcessHandler -> T.Text -> IO (Either Error T.Text)
timedCallandResponse time eph call = runExceptT $ do
  --give map
  TM.lift $ hPutStr (pstdin eph) $ (T.unpack call) ++ "\n"
  TM.lift $ hFlush (pstdin eph)

  -- check stdout
  check EndOfFile $ hIsEOF (pstdout eph)
  TM.lift $ putStrLn "Done checks"

  response <- ExceptT $ eitherTimeout time OutOfTime $ hGetLine (pstdout eph)

  return $ T.pack response

replaceExecutable :: T.Text -> ExternalProcessHandler -> IO ExternalProcessHandler
replaceExecutable path _ = createExternalProcess path

check :: Error-> IO Bool -> ExceptT Error IO ()
check er f = do
  open <- TM.lift f
  if open
  then ExceptT (return (Left er))
  else ExceptT (return (Right ()))

eitherTimeout :: Int -> Error -> IO a -> IO (Either Error a)
eitherTimeout time errorMsg action = do
  maybeResult <- timeout time action
  return $ fromMay maybeResult

  where
    fromMay Nothing  = Left errorMsg
    fromMay (Just r) = Right r
