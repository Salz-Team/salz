{-# LANGUAGE OverloadedStrings #-}
module BotBuilder ( buildBot
                  , BuildError
                  ) where

import qualified BotHandler as BH

import qualified System.IO.Temp as TF
import qualified System.Exit as SE
import qualified System.Process as SP
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Control.Exception as CE
import qualified Data.Text as T
import qualified Data.Either as E
import qualified Data.Either.Combinators as EC

-- File conflicts in the botDir are not checked for, old bots are not deleted
-- Also, there are no checks for anything, malicious build scripts are free
-- to do what they want, and incorrectly formated submissions are not checked
-- for.
-- this script MUST be rewritten before hosted on a server open to public

data BuildError = BuildError { unpack :: T.Text }
  deriving Show

instance CE.Exception BuildError

buildBot :: BH.Bot -> IO BH.Bot
buildBot (BH.UnBuilt pid tarPath) = E.either (\err -> BH.Crashed pid "" "" (T.unpack err))
                                             (\fp -> BH.NewBot pid fp (0, 0))
                                             <$> eitherErrPath
  where
    eitherErrPath = EC.mapLeft unpack <$> CE.try (buildBot_ tarPath)
buildBot b = return b

buildBot_ :: FilePath -> IO ( FilePath )
buildBot_ tarPath = TF.withSystemTempDirectory "build" $ \buildDir -> do
  (_, _, _, p1) <- SP.createProcess (SP.proc "tar" ["xf", tarPath]){ SP.cwd = Just buildDir}
  ec <- SP.waitForProcess p1
  _ <- if (ec == SE.ExitSuccess)
       then return () -- SP.createProcess (SP.proc "rm" [tarPath])
       else CE.throwIO $ BuildError $ "The file '" `T.append` (T.pack tarPath) `T.append` "' could not be extracted."

  let buildScriptPath = buildDir FP.</> "bot/build.sh"
  buildScriptExist <- D.doesFileExist buildScriptPath
  if buildScriptExist
  then do
    let buildCwd = FP.dropFileName buildScriptPath
    (_, _, _, p2) <- SP.createProcess (SP.proc "bash" [buildScriptPath]){SP.cwd = Just buildCwd}
    ec1 <- SP.waitForProcess p2
    if ec1 == SE.ExitSuccess
    then return ()
    else CE.throwIO $ BuildError $ "The build script from '" `T.append` (T.pack tarPath) `T.append` "' could not be built."
  else return ()

  targetDir <- TF.createTempDirectory "/tmp/" ""

  D.createDirectoryIfMissing True targetDir
  D.removeDirectoryRecursive targetDir
  D.createDirectoryIfMissing True targetDir

  (_, _, _, p3)<- SP.createProcess (SP.proc "cp" ["-r", buildDir FP.</> "bot", targetDir])
  _ <- SP.waitForProcess p3

  let  runScriptPath = targetDir FP.</> "bot" FP.</> "run.sh"
  runScriptExist <- D.doesFileExist runScriptPath
  if runScriptExist
  then return ()
  else CE.throwIO $ BuildError $ "The run script is missing." `T.append` ( T.pack runScriptPath)

  putStrLn $ "Done building bot, run script at:" ++ runScriptPath
  return ( runScriptPath )

-- Possible Exceptions:
-- just IOError?

-- Pattern:
-- catch exceptions, if there is an exception return a custom exception type
-- that contains an error msg
