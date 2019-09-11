{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database (saveGame) where
    
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time

import Prelude hiding (catch)
import Data.Time.LocalTime
import Data.Modular

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Types as MT


-- connectPostgreSQL uses the libpq connection string
-- For example:
--   "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpasswrd'"

data SaveStatus = Success | Failure

-- saveGame can throw exceptions from the Database.PostgreSQL.Simple class
-- these exceptions are not handled
saveGame :: T.Text -> MT.Game h w  -> IO ()
saveGame connectionString g = do
  conn <- connectPostgreSQL (TE.encodeUtf8 connectionString)

  let mquery = "INSERT INTO game (turnid, x, y, playerid, timestamp) Values (?,?,?,?,?)"
  time <- Finite <$> zonedTimeToLocalTime <$> getZonedTime :: IO (LocalTimestamp)
  let cells = (MT.bCells $ MT.board g)
  let turn = MT.turn g
  let rows = map (formatTurn turn time) cells
  executeMany conn mquery rows
  close conn
  return ()

formatTurn :: Int
           -> LocalTimestamp
           -> MT.Cell w h MT.CellInfo
           -> (Int, Int, Int, Int, LocalTimestamp)
formatTurn  turn time (MT.Cell x y (MT.CellInfo i)) = (turn, unMod x, unMod y, i, time)
