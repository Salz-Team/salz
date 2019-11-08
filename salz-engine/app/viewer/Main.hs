{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Main where

import System.Environment
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Types
import Database
import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

main :: IO ()
main = do
  args <- getArgs
  let dbfilepath = args!!0
  initBoard <- getBoard (Right dbfilepath) 4 :: IO MBoard
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initBoard



app :: App MBoard Tick Name
app = App { appDraw=drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }



type MBoard = Board 100 100 CellInfo

data Tick = Tick

type Name = ()

handleEvent :: MBoard -> BrickEvent Name Tick -> EventM Name (Next MBoard)
handleEvent board (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt board
handleEvent board (VtyEvent (V.EvKey V.KEsc [])) = halt board
handleEvent board _ = continue board


drawUI :: MBoard -> [Widget Name]
drawUI board = [ C.center $ drawBoard board ]

drawBoard :: MBoard -> Widget Name
drawBoard board = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "salz")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [99,98..0]]
    cellsInRow y = [drawCoord (x, y) | x <- [0..99]]
    drawCoord = drawCell . cellAt
    cellAt _ = Cell  0 0 (CellInfo 0)

drawCell :: Cell 100 100 CellInfo -> Widget Name
drawCell _ = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  ]
