{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database ( saveBoard
                , getBuildCmds
                , savePlayers ) where
    
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Types as PSQL.Types

import qualified Database.SQLite.Simple as SQLT

import Data.Time.Clock
import Data.Modular

import qualified Control.Concurrent as CC
import qualified Data.Either as E
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Types as MT
import qualified Control.Exception as CE

type AConnection = Either PSQL.Connection SQLT.Connection


getBuildCmds :: T.Text -> IO [(MT.PlayerId, FilePath)]
getBuildCmds cs = do
  players <- readPlayers cs
  let trimmed = map (\(p, _, _, updateBot, botDir, _) -> (p, updateBot, botDir)) players
  let valid = M.catMaybes $ map toMaybe trimmed

  con <- aConnectRepeat cs
  _ <- writeBuild con
  aClose con

  return $ map (\(a, _, c) -> (a, c)) valid
  where
    toMaybe :: (a, Maybe b, Maybe c) -> Maybe (a, b, c)
    toMaybe (_, Nothing, _) = Nothing
    toMaybe (_, _, Nothing) = Nothing
    toMaybe (a, Just b, Just c) = Just (a, b, c)

    query = "UPDATE players SET updatedbot = False;"
    writeBuild :: AConnection -> IO ()
    writeBuild con1 = aExecute con1 query() >> return ()

-- save status if there is one and save the bot location if there is one
savePlayers :: T.Text -> [MT.Player] -> IO ()
savePlayers cs players = do
  con <- aConnectRepeat cs

  let botHandlers = map (\p -> (MT.pPlayerId p, MT.eph $ MT.pBotHandler p)) players

  _ <- mapM (writeStatus con) botHandlers
  aClose con
  return ()
  where
    errorQuery = "UPDATE players SET botstatus = ? WHERE playerid = ?;"
    writeStatus :: AConnection -> (Int, E.Either T.Text a) -> IO ()
    writeStatus conn1 (i, Left errMsg) = aExecute conn1 errorQuery(errMsg, i) >> return ()
    writeStatus _ _ = return ()


saveBoard :: T.Text -> Int -> MT.Board h w MT.CellInfo -> IO ()
saveBoard connectionString turn board = do
  conn <- aConnectRepeat connectionString

  let mquery = "INSERT INTO game (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?)"
  time <- getCurrentTime
  let cells = (MT.bCells board)
  let rows = map (formatTurn turn time) cells
  aExecuteMany conn mquery rows
  aClose conn
  return ()


--
-- Dual Database Library
--

aConnect :: T.Text -> IO AConnection
aConnect "" = Right <$> SQLT.open (T.unpack "salz.db")
aConnect cs = Left <$> PSQL.connectPostgreSQL (TE.encodeUtf8 cs)

aConnectRepeat :: T.Text -> IO AConnection
aConnectRepeat t = CE.catch (aConnect t) handle
  where
    handle :: IOError -> IO AConnection
    handle _ = do
      putStrLn "Attempting to connect to database"
      CC.threadDelay 1000000
      aConnectRepeat t

aClose :: AConnection -> IO ()
aClose (Right con) = SQLT.close con
aClose (Left con) = PSQL.close con

aExecute :: (SQLT.ToRow q, PSQL.ToRow q) => AConnection -> T.Text -> q -> IO ()
aExecute (Right con) query item = SQLT.execute con (SQLT.Query query) item >> return ()
aExecute (Left con) query item = PSQL.execute con (PSQL.Types.Query (TE.encodeUtf8 query)) item >> return ()

aExecuteMany :: (SQLT.ToRow q, PSQL.ToRow q) => AConnection -> T.Text -> [q] -> IO ()
aExecuteMany (Right con) query lst = SQLT.executeMany con (SQLT.Query query) lst >> return ()
aExecuteMany (Left con) query lst = PSQL.executeMany con (PSQL.Types.Query (TE.encodeUtf8 query)) lst >> return ()
 
aQuery_ :: (SQLT.FromRow r, PSQL.FromRow r) => AConnection -> T.Text -> IO [r]
aQuery_  (Right con) query = SQLT.query_ con (SQLT.Query query)
aQuery_  (Left con) query = PSQL.query_ con (PSQL.Types.Query (TE.encodeUtf8 query))

-- connectPostgreSQL uses the libpq connection string
-- For example:
--   "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpasswrd'"


-- saveGame can throw exceptions from the Database.PostgreSQL.Simple class
-- these exceptions are not handled

formatTurn :: Int
           -> UTCTime
           -> MT.Cell w h MT.CellInfo
           -> (Int, Int, Int, Int, UTCTime)
formatTurn  turn time (MT.Cell x y (MT.CellInfo i)) = (turn, unMod x, unMod y, i, time)

-- saveGame can throw exceptions from the Database.PostgreSQL.Simple class
-- these exceptions are not handled
-- (playerid, username, botdir, updatedbot, newbotdir, botstatus)
readPlayers :: T.Text -> IO ([(Int, Maybe T.Text, Maybe FilePath, Maybe Bool, Maybe FilePath, Maybe T.Text)])
readPlayers connectionString = do
  conn <- aConnectRepeat connectionString
  let mquery = "SELECT * FROM players"
  result <- aQuery_ conn mquery
  aClose conn
  return result
