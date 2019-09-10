{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Database where
    
import Control.Applicative
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple
import Data.Modular
import qualified Data.Text as T
import Types



-- instance FromRow Cell where
--     fromRow = TestField <$> field <*> field
-- 
-- conn <- open "test.db"
-- execute conn "INSERT INTO test (str) VALUES (?)"
--   (Only ("test string 2" :: String))
-- r <- query_ conn "SELECT * from test" :: IO [TestField]
-- mapM_ print r
-- close conn
-- 

-- addTurn dbstring g turn = do
--     conn <- open dbstring
--     execute_ conn "CREATE TABLE board_(?) (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, owner INTEGER)"
-- 

-- deriving instance Show (Cell w h CellInfo)
instance ToRow (Cell w h CellInfo)
  where
    toRow (Cell x y (CellInfo i)) = toRow (unMod x, unMod y, i)


-- instance

saveGame :: T.Text -> Game w h -> IO ()
saveGame dbstring g = do
  conn <- open (T.unpack dbstring)
  let t = turn g
  execute_ conn $ Query $ "CREATE TABLE board_" `T.append` (T.pack (show t)) `T.append` " (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, owner INTEGER)"
  let cl = bCells $ board g
  mapM (saveCell conn t) cl
  close conn
  return ()

saveCell :: Connection -> Int -> Cell w h CellInfo -> IO ()
saveCell conn t c = do
  let query = Query $ "INSERT INTO board_" `T.append` (T.pack (show t)) `T.append` " (x, y, owner) Values (?,?,?)"
  execute conn query c
  return ()


-- return (player id, username, currentbot, oldbot)
-- readPlayers :: T.Text -> IO ((Int, Text, Text, Text))
-- readPlayers dbstring = do
--   conn <- open (T.unpack dbstring)
--   r <- query_ conn "SELECT * from players" :: IO []
