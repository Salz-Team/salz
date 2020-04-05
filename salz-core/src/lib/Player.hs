module Player ( applyCommands
              , getPlayerCells
              , filterLegalCommands
              ) where

import Board
import Types

import GHC.TypeLits hiding (Mod)
import Data.Modular


getPlayerCells :: Board h w CellInfo -> PlayerId -> [Cell h w CellInfo]
getPlayerCells b pi1 = getCellsBy b (\x -> pi1 == (cPlayerId x))

getOtherPlayersCells :: Board h w CellInfo -> PlayerId -> [Cell h w CellInfo]
getOtherPlayersCells b pi1 = getCellsBy b (\x -> pi1 /= (cPlayerId x))

isMyNeighbour :: (KnownNat w, KnownNat h) => Board w h CellInfo -> PlayerId -> Cell w h CellInfo -> Bool
isMyNeighbour board pid cell = any (\x -> dist x cell < 3) $ getPlayerCells board pid

isNearEnemy :: (KnownNat w, KnownNat h) => Board w h CellInfo -> Cell w h CellInfo -> Bool
isNearEnemy board cell = any (\x -> dist x cell < 30) $ getOtherPlayersCells board (cPlayerId  $ cItem cell)

isCommandLegal :: (KnownNat w, KnownNat h) => Board w h CellInfo -> PlayerId -> Command -> Bool
isCommandLegal board pid (Flip x y) = isMyNeighbour board pid cell && (not $ isNearEnemy board cell)
  where
    cell = Cell (toMod x) (toMod y) (CellInfo pid)

getLegalCommands :: (KnownNat w, KnownNat h) => Board w h CellInfo -> [(Player, [Command])] -> [(PlayerId, [Command])]
getLegalCommands board cmds = map getLegalCommand cmds
  where
    getLegalCommand (p, clst) = (pPlayerId p, filter (isCommandLegal board (pPlayerId p)) $ take 3 clst)

filterLegalCommands :: (KnownNat w, KnownNat h) => Board w h CellInfo -> [(Player, [Command])] -> [(PlayerId, Int, Int)]
filterLegalCommands board cmds = foldr foldHelper [] $ getLegalCommands board cmds
  where
    foldHelper :: (PlayerId, [Command]) -> [(PlayerId, Int, Int)] -> [(PlayerId, Int, Int)]
    foldHelper (pid, lst) res = (map (\(Flip x y) -> (pid, x, y)) lst) ++ res

applyFlips :: (KnownNat w, KnownNat h) => Board w h CellInfo -> [(PlayerId, [Command])] -> Board w h CellInfo
applyFlips board cmds = foldl togglePlayerCells board cmds
  where
    togglePlayerCells board1 (pid, clst) = foldl toggleCell board1 $ map (\(Flip x y) -> Cell (toMod x) (toMod y) (CellInfo pid)) clst

applyCommands :: (KnownNat w, KnownNat h) => Board w h CellInfo -> [(Player, [Command])] -> Board w h CellInfo
applyCommands board cmds = applyFlips board $ getLegalCommands board cmds
