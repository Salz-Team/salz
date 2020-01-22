{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database ( getLastSnapshotTurn
                , getSnapshot
                , getMoves
                , getBuildCmds
                , savePlayersStatus ) where
    
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Types as PSQL.Types

import qualified Database.SQLite.Simple as SQLT

import Data.Time.Clock
import Data.Modular
import GHC.TypeLits hiding (Mod)

import qualified Control.Concurrent as CC
import qualified Data.Either as E
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Types as MT
import qualified Control.Exception as CE

type AConnection = Either PSQL.Connection SQLT.Connection
type ConString = Either T.Text FilePath

getLastSnapshotTurn :: ConString -> IO (Maybe Int)
getLastSnapshotTurn cs = do
  con <- aConnectRepeat cs
  let mquery = "SELECT MAX(turnid) from snapshots;"
  result <- aQuery_ con mquery
  aClose con
  return $ head $ head result

getSnapshot :: (KnownNat w, KnownNat h) => ConString -> Int -> IO (MT.Board w h MT.CellInfo)
getSnapshot cs turn = do
  con <- aConnectRepeat cs
  let mquery = "SELECT x, y, playerid FROM snapshots WHERE turnid = ?;"
  result <- aQuery con mquery [turn]
  aClose con
  let cells = readCells result
  return $ MT.Board cells
  where
    readCells ::(KnownNat w, KnownNat h) => [(Maybe Int, Maybe Int, Maybe Int)] -> [MT.Cell w h MT.CellInfo]
    readCells cl = map readCell $ M.catMaybes $ map liftList cl

    readCell :: (KnownNat w, KnownNat h) => (Int, Int, Int) -> MT.Cell w h MT.CellInfo
    readCell (x, y, pid) = MT.Cell (toMod x) (toMod y) (MT.CellInfo pid)

    liftList :: (Maybe Int, Maybe Int, Maybe Int) -> Maybe (Int, Int, Int)
    liftList (Just a, Just b, Just c) = Just (a, b, c)
    liftList _ = Nothing

-- Types are turn Min -> turn Max -> [(turnid, x, y, playerid)]
getMoves ::  ConString -> Int -> Int -> IO [(Int, Int, Int, MT.PlayerId)]
getMoves cs min max = do
  con <- aConnectRepeat cs
  let mquery = "SELECT turnid, x, y, playerid FROM moves WHERE turnid BETWEEN ? AND ?;"
  result <- aQuery con mquery [min, max]
  aClose con
  return $ M.catMaybes $ map liftList result
  where
    liftList :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> Maybe (Int, Int, Int, MT.PlayerId)
    liftList (Just a, Just b, Just c, Just d) = Just (a, b, c, d)
    liftList _ = Nothing
    
-- (playerid, username, updatedbot, botstatus)
readPlayers :: ConString -> IO ([(Int, Maybe T.Text, Maybe FilePath, Maybe T.Text)])
readPlayers connectionString = do
  conn <- aConnectRepeat connectionString
  let mquery = "SELECT * FROM players"
  result <- aQuery_ conn mquery
  aClose conn
  return result
  
getBuildCmds :: ConString -> IO [(MT.PlayerId, FilePath)]
getBuildCmds cs = do
  players <- readPlayers cs

  con <- aConnectRepeat cs
  _ <- writeBuild con
  aClose con

  return $ M.catMaybes $ map toBuildCmds players
  where
    toBuildCmds :: (a, b, Maybe FilePath, c) -> Maybe (a, FilePath)
    toBuildCmds (_, _, Just "", _) = Nothing
    toBuildCmds (a, _, Just b, _) = Just (a, b)
    toBuildCmds _ = Nothing
    
    query = "UPDATE players SET botdir = '';"
    writeBuild :: AConnection -> IO ()
    writeBuild con1 = aExecute con1 query() >> return ()

savePlayersStatus :: ConString -> [MT.Player] -> IO ()
savePlayersStaus cs players = do
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


saveSnapshot :: ConString -> Int -> MT.Board h w MT.CellInfo -> IO ()
saveSnapshot cs turn board = do
  conn <- aConnectRepeat cs

  aExecute conn "CREATE TABLE IF NOT EXISTS snapshots (id SERIAL PRIMARY KEY, turnid INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP);"()

  let mquery = "INSERT INTO snapshots (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let cells = (MT.bCells board)
  let rows = map (formatTurn turn time) cells
  aExecuteMany conn mquery rows
  aClose conn
  return ()

saveMoves :: ConString -> turn -> [(PlayerId, Int, Int)] -> IO ()
saveMoves cs turn moves = do
  conn <- aConnectRepeat cs
  
  aExecute conn "CREATE TABLE IF NOT EXISTS moves (id SERIAL PRIMARY KEY, turnid INTEGER, x INTEGER, y INTEGER, playerid INTEGER, generated_at TIMESTAMP);"()
  
  let mquery = "INSERT INTO moves (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let formatedMoves = formatMoves turn time moves
  
  aExecuteMany conn mquery)
  aClose conn
  return ()
  where
    formatMoves :: Int -> UTCTime -> [(PlayerId, Int, Int)] -> [(Int, Int, Int, PlayerId, UTCTime)]
    formatMoves turn time moves = map  -- TODO
  
  

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

formatTurn :: Int
           -> UTCTime
           -> MT.Cell w h MT.CellInfo
           -> (Int, Int, Int, Int, UTCTime)
formatTurn  turn time (MT.Cell x y (MT.CellInfo i)) = (turn, unMod x, unMod y, i, time)

