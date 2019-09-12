module BotBuilder ( buildBot
                  ) where

import qualified Data.Text as T
import qualified Data.Either as E

-- botdir
buildBot :: T.Text -> IO ( E.Either T.Text T.Text )
buildBot newDir = return ( E.Right newDir )
