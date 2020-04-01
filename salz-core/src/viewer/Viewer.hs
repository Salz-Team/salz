module Viewer ( startViewer
              ) where


import qualified ViewerOptions as VO
import qualified ViewerState as VS
import qualified DataBase as DB


startViewer :: VO.ViewerCmdLineArgs -> IO ()
startViewer args = do
  board <- loadGame 0 (0, 0)
  let initialState = loadGame Limited (MapArea 0 0 100 100) (TurnRange 0 50) ()
  return ()
  


loadGame :: FilePath -> Limited x -> IO (Limited Board)
loadGame fp (Limited ma tr _) = do
  snapshotTurns <- getSnapshotTurns (Right fp)
  
  where
    firstTurn = VS.firstTurn tr
    firstTurn = VS.Turn tr
    
  
-- take the the closest snapshot before the firstTurn and then calculate as much as you need for
-- the lastTurn or untill the next snapshot
