module EventHandler where

import qualified Graphics.Vty as V
import Brick
import ViewerState


handleEvent :: ViewerState -> BrickEvent Name Tick -> EventM Name (Next ViewerState)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue (state {turn = (turn state)-1})
handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (state {turn = (turn state)+1})
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state _ = continue state

