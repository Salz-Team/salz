module Utils where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO

flushedPutStrLnB :: Handle -> LB.ByteString -> IO ()
flushedPutStrLnB handle line = do
  B.hPutStrLn handle (LB.toStrict line)
  hFlush handle
