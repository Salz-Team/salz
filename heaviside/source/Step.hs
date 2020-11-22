module Step ( step ) where

import qualified Database.PostgreSQL.Simple as DB

step :: DB.Connection -> IO ()
step conn = do
  turn <- getMostRecentTurn conn
  lastFrame <- getFrame conn turn
  let newFrame = stepFunction lastFrame
  putFrame newFrame

getMostRecentTurn :: DB.Connection -> IO Int
getMostRecentTurn con = return ()

getFrame :: DB.Connection -> Int -> IO [(Int, Int, Int)]
getFrame con turn = do
  let mquery = "SELECT x, y, playerid FROM game WHERE turnid = ?;"

  -- result:: [(Maybe Int, Maybe Int, Maybe Int)]
  result <- aQuery con mquery [turn]

putFrame :: DB.Connection -> [(Int, Int, Int)] -> IO ()
putFrame con frames = do
  let mquery = "INSERT INTO game (turnid, x, y, playerid, generated_at) Values (?,?,?,?,?);"
  time <- getCurrentTime
  let cells = map (\(x, y, pid) -> ())
  aExecuteMany conn mquery cells
  return ()

stepFunction :: [(Int, Int, Int)] -> [(Int, Int, Int)]
stepFunction = id


