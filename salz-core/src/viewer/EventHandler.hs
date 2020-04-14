module EventHandler where

import qualified ViewerState as VS
import Step
import Player
import qualified Map as Map
import qualified Database as DB

import qualified Graphics.Vty as V
import Brick
import Data.List

type Name = ()
data Tick = Tick

handleEvent :: VS.ViewerState -> BrickEvent Name Tick -> EventM Name (Next VS.ViewerState)
handleEvent state (AppEvent Tick                      ) = continue (appTick state)
handleEvent state (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue (state {VS.play = not (VS.play state)})

handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue (stepTurn state)

-- move the view
handleEvent state (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue (move 'w' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 's') [])) = continue (move 's' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue (move 'd' state)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue (move 'a' state)

handleEvent state (VtyEvent (V.EvKey (V.KChar 'l') [])) = suspendAndResume (loadErrLog state)

handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state _ = continue state

loadErrLog :: VS.ViewerState -> IO VS.ViewerState
loadErrLog state = do
  errlogs <- DB.getErrorLogs (Right (VS.dbfilepath state)) (VS.turn state)
  print (VS.turn state)
  print errlogs
  return $ state {VS.errlogs = Just errlogs}




stepTurn :: VS.ViewerState -> VS.ViewerState
stepTurn vs = vs { VS.turn = (VS.turn vs)+1
                 , VS.board = board2
                 , VS.errlogs = Nothing
                 }
  where
    board1 = applyCommands (VS.board vs) (unpackMoves (VS.moves vs) ((VS.turn vs)+1))
    board2 = step board1

    unpackMoves :: [(Int, Int, Int, Int)] -> Int -> [(Map.Coord, Int)]
    unpackMoves moves turn = map (\(_,x,y,pid) -> (Map.toCoord (x, y), pid)) $ filter (\(a,_,_,_) -> a == turn) moves



move :: Char -> VS.ViewerState -> VS.ViewerState
move 'w' state = state {VS.location = (fst (VS.location state), (snd (VS.location state)) + 5 )}
move 's' state = state {VS.location = (fst (VS.location state), (snd (VS.location state)) - 5 )}
move 'a' state = state {VS.location = ((fst (VS.location state)) - 5, snd (VS.location state))}
move 'd' state = state {VS.location = ((fst (VS.location state)) + 5, snd (VS.location state))}


appTick :: VS.ViewerState -> VS.ViewerState
appTick state = if (VS.play state) == False
                then state
                else (stepTurn state)
