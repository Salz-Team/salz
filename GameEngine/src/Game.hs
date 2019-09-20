{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Game where
    
import Player
import qualified PlayerBotHandler as PBH
import Board
import Step
import Types
import qualified BotBuilder as BB
import qualified Database as DB
import qualified Data.Text as T
import qualified Control.Monad as CM
import GHC.TypeLits hiding (Mod)
import Data.Maybe
import qualified Data.Either as E
import Data.Modular


gameLoop :: (KnownNat w, KnownNat h) => Game w h -> IO ()
gameLoop g = do
  dbplayerinfo <- DB.readPlayers (dbconnstring g)
  buildstatus <- buildNewBots (botDir g) dbplayerinfo
  DB.writeBuildResults (dbconnstring g) buildstatus

  -- Start new players bots, replace old bots with new bots
  g1 <- updatePlayerBotHandlers g buildstatus
  g2 <- createNewPlayerStarts g1

  botResults <- botTurns g2

  DB.writeBotResults $ botResults

  let commands = getLegalCommands botResults
  let g3 = applyCommands g2 commands

  let g4 = stepGame g3

  gameLoop g4

-- [(playerid, username, botdir, updatedbot, newbotdir, botstatus)]
buildNewBots :: FilePath -> [(Int, T.Text, FilePath, Bool, FilePath, T.Text)] -> IO ([(Int, E.Either T.Text FilePath)])
buildNewBots botDir playerinfo  = mapM buildBot $ filter isNewBots playerinfo
  where
    isNewBots :: (Int, T.Text, FilePath, Bool, FilePath, T.Text) -> Bool
    isNewBots (_, _, _, b, _, _) = b

    buildBot :: (Int, T.Text, FilePath, Bool, FilePath, T.Text) -> IO (Int, E.Either T.Text FilePath)
    buildBot (pid, _, _, _, nbdir, _) = (pid, ) <$> BB.buildBot nbdir botDir


updatePlayerBotHandlers :: Game w h -> [(Int, E.Either T.Text FilePath)] -> IO (Game w h)
updatePlayerBotHandlers g results = CM.foldM PBH.updatePlayerBot g $ E.rights $ map wrapInEither results
  where
    wrapInEither (pid, Left msg) = Left (pid, msg)
    wrapInEither (pid, Right path) = Right (pid, path)


createNewPlayerStarts :: Game w h -> IO (Game w h)
createNewPlayerStarts = undefined

botTurns :: Game w h -> IO ([(Int, E.Either T.Text [Command])])
botTurns _ = return []

getLegalCommands :: [(Int, E.Either T.Text [Command])] -> [(Int, Command)]
getLegalCommands = undefined

applyCommands :: Game w h -> [(Int, Command)] -> Game w h
applyCommands = undefined

stepGame :: Game w h -> Game w h
stepGame = undefined

playerTurn :: (KnownNat w, KnownNat h) => Game w h -> IO ([Either T.Text [Command]])
playerTurn g = do
  mapM playerTurn1 (players g)
  where
    playerTurn1 (p, pbh) = PBH.playerTakeTurn (playerVisibleBoard p) pbh
    playerVisibleBoard p = (Board (filter (isVisible (board g) (pPlayerId p)) $ (bCells (board g))))

kickBadPlayers :: Game w h -> [Either T.Text a] -> (Game w h, [a])
kickBadPlayers g pcl = (g {players = nps g}, cmds g)
  where
    zippedPlayersCommands g1 = zipWith (\ma b -> (\a -> (a, b)) <$> ma)  pcl $ players g1
    filteredPlayers g1 = E.rights $ zippedPlayersCommands g1
    nps g1 = map snd $ filteredPlayers g1
    cmds g1 = map fst $ filteredPlayers g1

applyCommands2 :: (KnownNat h, KnownNat w) => Game h w -> [[Command]] -> Game h w
applyCommands2 g cl = g {board = nboard}
  where
    nboard = foldl (applyCommand (board g)) (board g) $ zip (map (pPlayerId . fst) $ players g) cl


applyCommand :: (KnownNat h, KnownNat w) => Board h w CellInfo -> Board h w CellInfo -> (PlayerId ,[Command]) -> Board h w CellInfo
applyCommand ob b1 (p, cl) = foldr (\c b2 -> toggleCell b2 c) b1 $ filter (\c -> isFlipable ob c p) $ toCell p cl
  where
    toCell p1 cs = map (\(Flip x y) -> Cell (toMod x) (toMod y) (CellInfo p1)) cs
