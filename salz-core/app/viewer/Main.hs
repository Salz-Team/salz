{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Main where

import System.Environment
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Modular
import Types
import Board
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
  turns <- mapM (getTurn dbfilepath) [0..1000]
  let initState = State turns 0 dbfilepath
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initState

getTurn :: FilePath -> Int -> IO MBoard
getTurn fp turn = getBoard (Right fp) turn :: IO MBoard

app :: App State Tick Name
app = App { appDraw=drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

type MBoard = Board 100 100 CellInfo

data State = State
  { game :: [MBoard]
  , turn :: Int
  , filepath :: FilePath
  }

data Tick = Tick

type Name = ()

handleEvent :: State -> BrickEvent Name Tick -> EventM Name (Next State)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue (state {turn = (turn state)-1})
handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (state {turn = (turn state)+1})
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state _ = continue state


currentBoard :: State -> MBoard
currentBoard state = (game state)!!(turn state)

drawUI :: State -> [Widget Name]
drawUI state = [ C.center $ drawBoard (currentBoard state) ]

drawBoard :: MBoard -> Widget Name
drawBoard board = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "salz")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [99,98..0]]
    cellsInRow y = [drawCoord (Cell (toMod x) (toMod y) ()) | x <- [0..99]]
    drawCoord = drawCell . (getCellAt board)

drawCell :: Maybe (Cell w h CellInfo) -> Widget Name
drawCell (Just (Cell _ _ (CellInfo pid))) = withAttr (attrName $ fullAttr ++ (show (mod pid 6))) cw
drawCell Nothing = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

emptyAttr = "emptyAttr"
fullAttr = "fullAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName $ fullAttr ++ "0", V.blue `on` V.blue)
  , (attrName $ fullAttr ++ "1", V.red `on` V.red)
  , (attrName $ fullAttr ++ "2", V.green `on` V.green)
  , (attrName $ fullAttr ++ "3", V.cyan `on` V.cyan)
  , (attrName $ fullAttr ++ "4", V.yellow `on` V.yellow)
  , (attrName $ fullAttr ++ "5", V.magenta `on` V.magenta)
  ]
