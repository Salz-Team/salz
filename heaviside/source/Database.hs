{-# LANGUAGE OverloadedStrings #-}
module Database ( getMostRecentTurn
                , getFrame
                , putFrame
                ) where

import qualified Database.PostgreSQL.Simple as DB
import qualified Data.Maybe as M
import Data.Time.Clock

--------------------------------------------------------------------------------
-- Database Interface
--------------------------------------------------------------------------------
getMostRecentTurn :: DB.Connection -> IO Int
getMostRecentTurn conn = do
  let mquery = "SELECT MAX(turnid) FROM game;"
  result <- DB.query_ conn mquery
  return $ M.fromMaybe 0 $ head $ head result

getFrame :: DB.Connection -> Int -> IO [(Int, Int, Int)]
getFrame conn turn = do
  let mquery = "SELECT x, y, playerid FROM game WHERE turnid = ?;"

  result <- DB.query conn mquery [turn]

  return $ M.catMaybes $ map liftList result
  where
    liftList :: (Maybe Int, Maybe Int, Maybe Int) -> Maybe (Int, Int, Int)
    liftList (Nothing, _, _) = Nothing
    liftList (_, Nothing, _) = Nothing
    liftList (_, _, Nothing) = Nothing
    liftList (Just a, Just b, Just c) = Just (a, b, c)

putFrame :: DB.Connection -> Int -> [(Int, Int, Int)] -> IO ()
putFrame conn turn frames = do
  let mquery = "INSERT INTO game (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let cells = map (formatTurn time turn) frames
  DB.executeMany conn mquery cells
  return ()
  where
    formatTurn :: UTCTime -> Int -> (Int, Int, Int) -> (Int, Int, Int, Int, UTCTime)
    formatTurn time turn (x, y, pid) = (turn, x, y, pid, time)

