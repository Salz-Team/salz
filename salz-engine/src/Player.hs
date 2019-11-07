module Player ( applyCommands
              , getPlayerCells
              ) where

import Board
import Types

import GHC.TypeLits hiding (Mod)
import Data.Modular



isVisible :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> PlayerId -> Cell h w CellInfo -> Bool
isVisible b p c = any (\x -> dist x c < 3) $ getPlayerCells b p

isFlipable :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> Cell h w a -> PlayerId -> Bool
isFlipable b c p = any (\x -> dist x c < 3) $ getPlayerCells b p

getPlayerCells :: Board h w CellInfo -> PlayerId -> [Cell h w CellInfo]
getPlayerCells b pi1 = getCellsBy b (\x -> pi1 == (cPlayerId x))

isCommandValid :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> PlayerId -> Command -> Bool
isCommandValid b pi1 (Flip x y) = isFlipable b (Cell (toMod x) (toMod y) ()) pi1

getLegalCommands :: Board w h CellInfo -> [Command] -> [Command]
getLegalCommands = undefined

applyCommands :: Board w h CellInfo -> [(Player, [Command])] -> Board w h CellInfo
applyCommands = undefined
