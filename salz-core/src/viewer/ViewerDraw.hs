{-# LANGUAGE DataKinds, OverloadedStrings #-}

module ViewerDraw where

import ViewerState
import Types
import Board

import Data.Modular
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

drawUI :: ViewerState -> [Widget ()]
drawUI state = [ C.center $ drawBoard (board state) ]

drawBoard :: Board 100 100 CellInfo -> Widget ()
drawBoard board = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "salz")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [99,98..0]]
    cellsInRow y = [drawCoord (Cell (toMod x) (toMod y) ()) | x <- [0..99]]
    drawCoord = drawCell . (getCellAt board)

drawCell :: Maybe (Cell w h CellInfo) -> Widget ()
drawCell (Just (Cell _ _ (CellInfo pid))) = withAttr (attrName $ fullAttr ++ (show (mod pid 6))) cw
drawCell Nothing = withAttr emptyAttr cw

cw :: Widget ()
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
