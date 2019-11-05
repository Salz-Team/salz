module Player where

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

packPlayerMap :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> Player -> PlayerMap
packPlayerMap b p = map toRelativeCoord $ filter (isVisible b pi1) (bCells b)
  where
    toRelativeCoord c = (unMod $ cX $ relativeCell c, unMod $ cY $ relativeCell c, cPlayerId $ cItem c)

    relativeCell = relativeCoordinates (Cell x y ())
    x = toMod $ fst $ pPlayerSource p
    y = toMod $ snd $ pPlayerSource p
    pi1 = pPlayerId p

isCommandValid :: (KnownNat w, KnownNat h) =>
  Board h w CellInfo -> PlayerId -> Command -> Bool
isCommandValid b pi1 (Flip x y) = isFlipable b (Cell (toMod x) (toMod y) ()) pi1

fillStartingLocation :: (KnownNat w, KnownNat h) =>
  Board w h CellInfo -> Player -> Board w h CellInfo
fillStartingLocation b p = Board { bCells = nCell:(bCells b) }
  where
    nCell = Cell x y (CellInfo pid)
    y = toMod $ snd $ pPlayerSource p
    x = toMod $ fst $ pPlayerSource p
    pid = pPlayerId p

