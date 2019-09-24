{-# LANGUAGE OverloadedStrings #-}
module BotBuilder ( buildBot
                  , BuildError
                  ) where

import qualified System.IO.Temp as TF
import qualified System.Exit as SE
import qualified System.Process as SP
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Control.Exception as CE
import qualified Data.Text as T
import qualified Data.Either as E

-- File conflicts in the botDir are not checked for, old bots are not deleted
-- Also, there are no checks for anything, malicious build scripts are free
-- to do what they want, and incorrectly formated submissions are not checked
-- for.
-- this script MUST be rewritten before hosted on a server open to public


data BuildError = BuildError T.Text
  deriving Show

instance CE.Exception BuildError

buildBot :: FilePath -> FilePath -> IO ( E.Either T.Text FilePath )
buildBot tarPath targetDir = translate <$> CE.try (buildBot_ tarPath targetDir)
  where
    translate (Left (BuildError t)) = Left t
    translate (Right fp) = Right fp

-- tarPath -> builddir -> finisheddir -> IO (either failmessage pathtorun.sh)
buildBot_ :: FilePath -> FilePath -> IO ( FilePath )
buildBot_ tarPath targetDir = TF.withSystemTempDirectory "build" $ \buildDir -> do
  (_, _, _, p1) <- SP.createProcess (SP.proc "tar" ["xf", tarPath]){ SP.cwd = Just buildDir}
  ec <- SP.waitForProcess p1
  if (ec == SE.ExitSuccess)
  then SP.createProcess (SP.proc "rm" [tarPath])
  else CE.throwIO $ BuildError $ "The file '" `T.append` (T.pack tarPath) `T.append` "' could not be extracted."

  let baseName = FP.dropExtensions (FP.takeFileName tarPath)

  let buildScriptPath = buildDir FP.</> baseName FP.</> "build.sh"
  buildScriptExist <- D.doesFileExist buildScriptPath
  if buildScriptExist
  then do
    let buildCwd = FP.dropFileName buildScriptPath
    (_, _, _, p2) <- SP.createProcess (SP.proc "bash" [buildScriptPath]){SP.cwd = Just buildCwd}
    ec1 <- SP.waitForProcess p2
    if ec1 == SE.ExitSuccess
    then return ()
    else CE.throwIO $ BuildError $ "The file '" `T.append` (T.pack tarPath) `T.append` "' could not be extracted."
  else return ()


  (_, _, _, p3)<- SP.createProcess (SP.proc "cp" ["-r", buildDir FP.</> baseName, targetDir])
  SP.waitForProcess p3

  let runScriptPath = targetDir FP.</> baseName FP.</> "run.sh"
  runScriptExist <- D.doesFileExist runScriptPath
  if runScriptExist
  then return ()
  else CE.throwIO $ BuildError $ "The run script is missing."

  return ( runScriptPath )

-- Possible Exceptions:
-- just IOError?

-- Pattern:
-- catch exceptions, if there is an exception return a custom exception type
-- that contains an error msg
