{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database ( getSnapshotTurns
                , getSnapshot
                , getMoves
                , getBuildCmds
                , saveSnapshot
                , saveMoves
                , getErrorLogs
                , saveErrorLogs
                , savePlayersStatus ) where

import qualified Map as Map
import qualified BotHandler as BH

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
import qualified Control.Exception as CE

type AConnection = Either PSQL.Connection SQLT.Connection
type ConString = Either T.Text FilePath

getSnapshotTurns :: ConString -> IO [Int]
getSnapshotTurns cs = do
  con <- aConnectRepeat cs
  let mquery = "SELECT DISTINCT turnid from snapshots;"
  result <- aQuery_ con mquery
  aClose con
  return $ liftList result
  where
    liftList :: [[Maybe Int]] -> [Int]
    liftList list = M.catMaybes $ concat list

getSnapshot :: ConString -> Int -> IO Map.Map
getSnapshot cs turn = do
  con <- aConnectRepeat cs
  let mquery = "SELECT x, y, playerid FROM snapshots WHERE turnid = ?;"
  result <- aQuery con mquery [turn]
  aClose con
  return $ readMap result
  where
    readMap ::[(Maybe Int, Maybe Int, Maybe Int)] -> Map.Map
    readMap cl = Map.M $ map readCell $  M.catMaybes $ map liftList cl

    readCell :: (Int, Int, Int) -> (Map.Coord, Int)
    readCell (x, y, pid) = (Map.Coord (toEnum x) (toEnum y), pid)

    liftList :: (Maybe Int, Maybe Int, Maybe Int) -> Maybe (Int, Int, Int)
    liftList (Just a, Just b, Just c) = Just (a, b, c)
    liftList _ = Nothing

-- Types are turn Min -> turn Max -> [(turnid, x, y, playerid)]
getMoves ::  ConString -> Int -> Int -> IO [(Int, Int, Int, Int)]
getMoves cs min max = do
  con <- aConnectRepeat cs
  let mquery = "SELECT turnid, x, y, playerid FROM moves WHERE turnid BETWEEN ? AND ?;"
  result <- aQuery con mquery [min, max]
  aClose con
  return $ M.catMaybes $ map liftList result
  where
    liftList :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> Maybe (Int, Int, Int, Int)
    liftList (Just a, Just b, Just c, Just d) = Just (a, b, c, d)
    liftList _ = Nothing
    
-- (playerid, username, updatedbot, botstatus)
readPlayers :: ConString -> IO ([(Int, Maybe T.Text, Maybe FilePath, Maybe T.Text, Maybe T.Text, Maybe T.Text)])
readPlayers connectionString = do
  conn <- aConnectRepeat connectionString
  let mquery = "SELECT * FROM players"
  result <- aQuery_ conn mquery
  aClose conn
  return result
  
getBuildCmds :: ConString -> IO [BH.Bot]
getBuildCmds cs = do
  players <- readPlayers cs

  con <- aConnectRepeat cs
  _ <- writeBuild con
  aClose con

  return $ M.catMaybes $ map toBuildCmds players
  where
    toBuildCmds :: (Int, b, Maybe FilePath, c, d, e) -> Maybe BH.Bot
    toBuildCmds (_, _, Just "", _, _, _) = Nothing
    toBuildCmds (a, _, Just b, _, _, _) = Just $ BH.UnBuilt a b
    toBuildCmds _ = Nothing
    
    query = "UPDATE players SET botdir = '';"
    writeBuild :: AConnection -> IO ()
    writeBuild con1 = aExecute con1 query() >> return ()

saveErrorLogs :: ConString-> [BH.Bot] -> Int ->  IO ()
saveErrorLogs cs bots turn = do
  con <- aConnectRepeat cs

  aExecute con "CREATE TABLE IF NOT EXISTS errorlogs (id SERIAL PRIMARY KEY, playerid INTEGER, turn INTEGER, botmemory VARCHAR, botstderr VARCHAR, errormsg VARCHAR);"()
  _ <- mapM (writeStatus con) bots
  aClose con
  return ()
  where
    errorQuery = "INSERT INTO errorlogs (playerid, turn, botmemory, botstderr, errormsg) VALUES (?,?,?,?,?);"
    writeStatus :: AConnection -> BH.Bot -> IO ()
    writeStatus conn1 (BH.Crashed pid errorLog memory errormsg) = aExecute conn1 errorQuery(pid, turn, memory, errorLog, errormsg) >> return ()
    writeStatus conn1 (BH.Bot pid _ memory errorLog _ _) = aExecute conn1 errorQuery(pid, turn, memory, errorLog, ("No error to report." :: String)) >> return ()
    writeStatus _ _ = return ()

getErrorLogs :: ConString -> Int ->  IO [(Int, String, String, String)]
getErrorLogs cs turn = do
  con <- aConnectRepeat cs
  let mquery = "SELECT playerid, botmemory, botstderr, errormsg FROM errorlogs WHERE turn = ?;"
  result <- aQuery con mquery [turn]
  aClose con
  return $ M.catMaybes $ map liftList result
  where
    liftList :: (Maybe Int, Maybe String, Maybe String, Maybe String) -> Maybe (Int, String, String, String)
    liftList (Just a, b, c, d) = Just (a, M.fromMaybe "" b, M.fromMaybe "" c, M.fromMaybe "" d)
    liftList _ = Nothing


savePlayersStatus :: ConString -> [BH.Bot] -> IO ()
savePlayersStatus cs bots = do
  con <- aConnectRepeat cs

  aExecute con "CREATE TABLE IF NOT EXISTS players (playerid SERIAL PRIMARY KEY, username VARCHAR, botdir VARCHAR, botmemory VARCHAR, botstderr VARCHAR, errormsg VARCHAR);"()
  _ <- mapM (writeStatus con) bots
  aClose con
  return ()
  where
    errorQuery = "UPDATE players SET botmemory = ?, botstderr = ?, errormsg = ? WHERE playerid = ?;"
    writeStatus :: AConnection -> BH.Bot -> IO ()
    writeStatus conn1 (BH.Crashed pid errorLog memory errormsg) = aExecute conn1 errorQuery(memory, errorLog, errormsg, pid) >> return ()
    writeStatus _ _ = return ()


saveSnapshot :: ConString -> Int -> Map.Map -> IO ()
saveSnapshot cs turn (Map.M mapLst) = do
  conn <- aConnectRepeat cs

  aExecute conn "CREATE TABLE IF NOT EXISTS snapshots (id SERIAL PRIMARY KEY, turnid INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP);"()

  let mquery = "INSERT INTO snapshots (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let rows = map (\(Map.Coord x y, pid) -> (turn, fromEnum x, fromEnum y, pid, time)) mapLst
  aExecuteMany conn mquery rows
  aClose conn
  return ()

saveMoves :: ConString -> Int -> [(Map.Coord, Int)] -> IO ()
saveMoves cs turn moves = do
  conn <- aConnectRepeat cs
  
  aExecute conn "CREATE TABLE IF NOT EXISTS moves (id SERIAL PRIMARY KEY, turnid INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP);"()
  
  let mquery = "INSERT INTO moves (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let formatedMoves = formatMoves turn time moves
  
  aExecuteMany conn mquery formatedMoves
  aClose conn
  return ()
  where
    formatMoves :: Int -> UTCTime -> [(Map.Coord, Int)] -> [(Int, Int, Int, Int, UTCTime)]
    formatMoves turn time = map (\(Map.Coord x y, pid) -> (turn, fromEnum x, fromEnum y, pid, time))
  
  

--
-- Dual Database Library
--

aConnect :: ConString -> IO AConnection
aConnect (Right fp) = Right <$> SQLT.open fp
aConnect (Left cs) = Left <$> PSQL.connectPostgreSQL (TE.encodeUtf8 cs)

aConnectRepeat :: ConString -> IO AConnection
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
 
aQuery :: (SQLT.FromRow r, PSQL.FromRow r, SQLT.ToRow q, PSQL.ToRow q) => AConnection -> T.Text -> q -> IO [r]
aQuery  (Right con) query lst = SQLT.query con (SQLT.Query query) lst
aQuery  (Left con) query lst = PSQL.query con (PSQL.Types.Query (TE.encodeUtf8 query)) lst

aQuery_ :: (SQLT.FromRow r, PSQL.FromRow r) => AConnection -> T.Text -> IO [r]
aQuery_  (Right con) query = SQLT.query_ con (SQLT.Query query)
aQuery_  (Left con) query = PSQL.query_ con (PSQL.Types.Query (TE.encodeUtf8 query))

-- connectPostgreSQL uses the libpq connection string
-- For example:
--   "host='localhost' port=5432 dbname='postgres' user='postgres' password='mysecretpasswrd'"


-- saveGame can throw exceptions from the Database.PostgreSQL.Simple class
-- these exceptions are not handled

