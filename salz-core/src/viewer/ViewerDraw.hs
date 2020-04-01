{-# LANGUAGE DataKinds, OverloadedStrings #-}

module ViewerDraw where

import qualified ViewerState as VS
import Types
import Board

import Data.Modular
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

drawUI :: VS.ViewerState -> [Widget ()]
drawUI state = [ C.center $ hBox $ [drawBoard state, vBox [drawStats state, drawMoves state]] ]

drawBoard :: VS.ViewerState -> Widget ()
drawBoard state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("salz"))
  $ vBox rows
  where
    board = VS.board state
    (vx, vy) = VS.location state
    rows = [hBox $ cellsInRow r | r <- [99,98..0]]
    cellsInRow y = [drawCoord (Cell (toMod (x+vx)) (toMod (y+vy)) ()) | x <- [0..99]]
    drawCoord = drawCell . (getCellAt board)

drawCell :: Maybe (Cell w h CellInfo) -> Widget ()
drawCell (Just (Cell _ _ (CellInfo pid))) = withAttr (attrName $ fullAttr ++ (show (mod pid 6))) cw
drawCell Nothing = withAttr emptyAttr cw

cw :: Widget ()
cw = str "  "

drawStats :: VS.ViewerState -> Widget ()
drawStats state = withBorderStyle BS.unicodeBold
 $ B.border
 $ hLimit 22
 $ vLimit 22
 $ vBox [ str ("Location: " ++ (show (VS.location state)))
        , str ("Turn: " ++ (show (VS.turn state)))
        ]

drawMoves :: VS.ViewerState -> Widget ()
drawMoves state = withBorderStyle BS.unicodeBold
 $ B.borderWithLabel (str "Bot moves")
 $ hLimit 22
 $ vBox prettyMoves
  where
    prettyMoves = map pretify $ filter (\(a,_,_,_) -> a == (VS.turn state)) (VS.moves state)
    pretify (_, x, y, pid) = hBox [ withAttr (attrName $ fullAttr ++ (show (mod pid 6))) (str " ")
                                  , str ("  " ++ show pid ++ "    " ++ show x ++ "  " ++ show y)
                                  ]

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
