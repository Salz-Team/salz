{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database ( saveGame
                , readPlayers
                , writeBuildResults
                , writeBotResults ) where
    
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time

import Prelude hiding (catch)
import Data.Time.LocalTime
import Data.Modular

import qualified Data.Either as E
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

  let mquery = "INSERT INTO game (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?)"
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

-- saveGame can throw exceptions from the Database.PostgreSQL.Simple class
-- these exceptions are not handled
-- (playerid, username, botdir, updatedbot, newbotdir, botstatus)
readPlayers :: T.Text -> IO ([(Int, T.Text, T.Text, Bool, T.Text, T.Text)])
readPlayers connectionString = do
  conn <- connectPostgreSQL (TE.encodeUtf8 connectionString)
  let mquery = "SELECT * FROM players"
  result <- query_ conn mquery
  close conn
  return result


writeBuildResults :: T.Text -> [(Int, E.Either T.Text T.Text)] -> IO ()
writeBuildResults connectionString buildresults = do
  conn <- connectPostgreSQL (TE.encodeUtf8 connectionString)
  mapM (writeResult conn) buildresults
  close conn
  return ()
  where
    errorQuery = "UPDATE players SET updatedbot = False, botstatus = ? WHERE playerid = ?;"
    successQuery = "UPDATE players SET updatedbot = False, botstatus = 'Successful Build', newbotdir = ? WHERE playerid = ?;"

    writeResult :: Connection -> (Int, E.Either T.Text T.Text) -> IO ()
    writeResult conn1 (i, Left errMsg) = execute conn1 errorQuery (errMsg, i) >> return ()
    writeResult conn1 (i, Right newPath) = execute conn1 successQuery (newPath, i) >> return ()

writeBotResults _ = return ()
