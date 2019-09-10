{-# LANGUAGE OverloadedStrings #-}

module Game where
    
import Player
import PlayerBotHandler
import Board
import Step
import Types
import qualified Database as B
import qualified Data.Text as T
import GHC.TypeLits hiding (Mod)
import Data.Maybe
import Data.Either
import Data.Modular

initializeGame :: (KnownNat w, KnownNat h) => [T.Text] -> IO (Game w h)
initializeGame playerExes = do

  -- random starting locations
  let startingLocations = map getStartLoc [1..(length playerExes)]

  let p = map (\(i, src) -> Player i src) $ zip [0..] startingLocations
  let b = foldr (\p1 b1 -> fillStartingLocation b1 p1) (Board []) p

  return $ Game b (zip p playerExes) 0

fillStartingLocation :: (KnownNat w, KnownNat h) =>
  Board w h CellInfo -> Player -> Board w h CellInfo
fillStartingLocation b p = Board { bCells = nCell:(bCells b) }
  where
    nCell = Cell x y (CellInfo pid)
    y = toMod $ snd $ pPlayerSource p
    x = toMod $ fst $ pPlayerSource p
    pid = pPlayerId p

getStartLoc :: Int -> (Int, Int)
getStartLoc seed = (seed * 5790153, seed * 57281)


gameLoop :: (KnownNat w, KnownNat h) => Game w h -> IO ()
gameLoop game = do
  commands <- playerTurn game

  let (filteredgame, filteredCommands) = kickBadPlayers game commands
  let ngame = applyCommands filteredgame filteredCommands
  let steppedgame = ngame {board = step (board ngame), turn = (turn ngame) + 1}
  B.saveGame "test.db" steppedgame

  putStrLn "Players Applied"
  putStrLn $ fromJust $ display (board ngame) 1 1 70 70
  putStrLn "Stepped"
  putStrLn $ fromJust $ display (board steppedgame) 1 1 70 70

  if (bCells $ board steppedgame) == []
  then putStrLn "Done"
  else gameLoop steppedgame

playerTurn :: (KnownNat w, KnownNat h) => Game w h -> IO ([Either ParseError [Command]])
playerTurn g = do
  mapM playerTurn1 (players g)
  where
    playerTurn1 (p, pbh) = playerTakeTurn (playerVisibleBoard p) pbh
    playerVisibleBoard p = (Board (filter (isVisible (board g) (pPlayerId p)) $ (bCells (board g))))

kickBadPlayers :: Game w h -> [Either ParseError a] -> (Game w h, [a])
kickBadPlayers g pcl = (g {players = nps g}, cmds g)
  where
    zippedPlayersCommands g1 = zipWith (\ma b -> (\a -> (a, b)) <$> ma)  pcl $ players g1
    filteredPlayers g1 = rights $ zippedPlayersCommands g1
    nps g1 = map snd $ filteredPlayers g1
    cmds g1 = map fst $ filteredPlayers g1

applyCommands :: (KnownNat h, KnownNat w) => Game h w -> [[Command]] -> Game h w
applyCommands g cl = g {board = nboard}
  where
    nboard = foldl (applyCommand (board g)) (board g) $ zip (map (pPlayerId . fst) $ players g) cl


applyCommand :: (KnownNat h, KnownNat w) => Board h w CellInfo -> Board h w CellInfo -> (PlayerId ,[Command]) -> Board h w CellInfo
applyCommand ob b1 (p, cl) = foldr (\c b2 -> toggleCell b2 c) b1 $ filter (\c -> isFlipable ob c p) $ toCell p cl
  where
    toCell p1 cs = map (\(Flip x y) -> Cell (toMod x) (toMod y) (CellInfo p1)) cs
