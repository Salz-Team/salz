module BotBuilder ( buildBot
                  ) where

import qualified System.IO.Temp as TF
import qualified System.Process as SP
import qualified System.FilePath as FP
import qualified Data.Text as T
import qualified Data.Either as E

-- File conflicts in the botDir are not checked for, old bots are not deleted
-- Also, there are no checks for anything, malicious build scripts are free
-- to do what they want, and incorrectly formated submissions are not checked
-- for.
-- this script MUST be rewritten before hosted on a server open to public

-- tarPath -> builddir -> finisheddir -> IO (either failmessage pathtorun.sh)
buildBot :: FilePath -> FilePath -> IO ( E.Either T.Text FilePath )
buildBot tarPath targetDir = TF.withSystemTempDirectory "build" $ \buildDir -> do
  (_, _, _, p1) <- SP.createProcess (SP.proc "tar" ["xf", tarPath]){ SP.cwd = Just buildDir}
  SP.waitForProcess p1
  -- SP.createProcess (SP.proc "rm" [tarPath])

  let baseName = FP.dropExtensions (FP.takeFileName tarPath)

  let buildScriptPath = buildDir FP.</> baseName FP.</> "build.sh"
  let buildCwd = FP.dropFileName buildScriptPath
  (_, _, _, p2) <- SP.createProcess (SP.proc "bash" [buildScriptPath]){SP.cwd = Just buildCwd}
  SP.waitForProcess p2

  (_, _, _, p3)<- SP.createProcess (SP.proc "cp" ["-r", buildDir FP.</> baseName, targetDir])
  SP.waitForProcess p3

  let runScriptPath = targetDir FP.</> baseName FP.</> "run.sh"

  return ( E.Right runScriptPath )
