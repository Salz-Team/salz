{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database where
    
import Control.Applicative
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.Modular
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Types as MT
import Data.Time.LocalTime


-- connectPostgreSQL uses the libpq connection string
-- For example:
--   "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpasswrd'"

data SaveStatus = Success | Failure

saveGame :: T.Text -> MT.Game h w  -> IO (SaveStatus)
saveGame connectionString g = do
  conn <- connectPostgreSQL (TE.encodeUtf8 connectionString) 
  let query = "INSERT INTO game (turnid, x, y, playerid, timestamp) Values (?,?,?,?,?)"
  time <- Finite <$> zonedTimeToLocalTime <$> getZonedTime :: IO (LocalTimestamp)
  let turn = MT.turn g
  let rows = map (formatTurn turn time) (MT.bCells $ MT.board g)

  rowsEffected <- executeMany conn query rows

  close conn
  return Success

formatTurn :: Int
           -> LocalTimestamp
           -> MT.Cell w h MT.CellInfo
           -> (Int, Int, Int, Int, LocalTimestamp)
formatTurn  turn time (MT.Cell x y (MT.CellInfo i)) = (turn, unMod x, unMod y, i, time)
