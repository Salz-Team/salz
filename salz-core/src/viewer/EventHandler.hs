module EventHandler where

import ViewerState
import Step
import Player
import qualified Map as Map

import qualified Graphics.Vty as V
import Brick
import Data.List

type Name = ()
data Tick = Tick

handleEvent :: ViewerState -> BrickEvent Name Tick -> EventM Name (Next ViewerState)
handleEvent state (AppEvent Tick                      ) = continue (appTick state)
handleEvent state (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue (state {play = not (play state)})

handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (stepTurn state)

-- move the view
handleEvent state (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue (move 'w' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 's') [])) = continue (move 's' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue (move 'd' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue (move 'a' state)

handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state _ = continue state


stepTurn :: ViewerState -> ViewerState
stepTurn vs = vs { turn = (turn vs)+1
                 , board = board2
                 }
  where
    board1 = applyCommands (board vs) (unpackMoves (moves vs) ((turn vs)+1))
    board2 = step board1

    unpackMoves :: [(Int, Int, Int, Int)] -> Int -> [(Map.Coord, Int)]
    unpackMoves moves turn = map (\(_,x,y,pid) -> (Map.toCoord (x, y), pid)) $ filter (\(a,_,_,_) -> a == turn) moves



move :: Char -> ViewerState -> ViewerState
move 'w' state = state {location = (fst (location state), (snd (location state)) + 5 )}
move 's' state = state {location = (fst (location state), (snd (location state)) - 5 )}
move 'a' state = state {location = ((fst (location state)) - 5, snd (location state))}
move 'd' state = state {location = ((fst (location state)) + 5, snd (location state))}


appTick :: ViewerState -> ViewerState
appTick state = if (play state) == False
                then state
                else (stepTurn state)
