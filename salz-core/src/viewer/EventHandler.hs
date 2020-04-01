module EventHandler where

import ViewerState
import Types
import Step
import Player

import qualified Graphics.Vty as V
import Brick
import Data.List

type Name = ()
data Tick = Tick

handleEvent :: ViewerState -> BrickEvent Name Tick -> EventM Name (Next ViewerState)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue (state {turn = (turn state)-1})
handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (stepTurn state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state _ = continue state


stepTurn :: ViewerState -> ViewerState
stepTurn vs = vs { turn = (turn vs)+1
                 , board = board2
                 }
  where
    board1 = applyCommands (board vs) (meltMoves $ unpackMoves (moves vs) (turn vs))
    board2 = step board1

    unpackMoves :: [(Int, Int, Int, PlayerId)] -> Int -> [(PlayerId, Command)]
    unpackMoves moves turn = map (\(_,x,y,pid) -> (pid, Flip x y)) $ filter (\(a,_,_,_) -> a == turn) moves

    meltMoves :: [(PlayerId, Command)] -> [(PlayerId, [Command])]
    meltMoves lin = map (\lst -> (fst (head lst), map snd lst)) $ groupBy (\a b -> (fst a) == (fst b)) lin


