{-# LANGUAGE DataKinds #-}
module Viewer ( startViewer
              ) where

import qualified ViewerOptions as VO
import qualified EventHandler  as EH
import qualified ViewerState as VS
import qualified ViewerDraw as VD
import qualified Database as DB
import qualified Map as Map

import Brick
import Brick.BChan
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V


startViewer :: VO.ViewerCmdLineArgs -> IO ()
startViewer args = do
  print (VO.dbFilePath args)
  initialState <- loadGame (VO.dbFilePath args)
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan EH.Tick
    threadDelay 100000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState



app :: App VS.ViewerState EH.Tick ()
app = App { appDraw=VD.drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = EH.handleEvent
          , appStartEvent = return
          , appAttrMap = const VD.theMap
          }


-- Get first snapshot and then load all of the commands untill the next snapshot

loadGame :: FilePath -> IO VS.ViewerState
loadGame fp = do
  snapshotTurns <- DB.getSnapshotTurns (Right fp)
  let firstTurn = if length snapshotTurns == 0 then 0 else head snapshotTurns

  snap <- DB.getSnapshot (Right fp) firstTurn
  moves <- DB.getMoves (Right fp) firstTurn (firstTurn+100)
  return VS.ViewerState
    { VS.turn = firstTurn
    , VS.play = False
    , VS.location = Map.C 0 0
    , VS.dbfilepath = fp
    , VS.board = snap
    , VS.moves = moves
    , VS.errlogs = Nothing
    }

