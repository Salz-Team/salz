{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Game where
    
import Player
import PlayerBotHandler
import Board
import Step
import Types
import qualified BotBuilder as BB
import qualified Database as DB
import qualified Data.Text as T
import GHC.TypeLits hiding (Mod)
import Data.Maybe
import qualified Data.Either as E
import Data.Modular


gameLoop :: (KnownNat w, KnownNat h) => Game w h -> IO ()
gameLoop g = do
  dbplayerinfo <- DB.readPlayers (dbconnstring g)
  builtstatus <- buildNewBots dbplayerinfo
  DB.writeBuildResults (dbconnstring g) builtstatus

  -- Start new players bots, replace old bots with new bots
  g1 <- updatePlayerBotHandlers g DB.readPlayers
  g2 <- createNewPlayerStarts g1

  botResults <- botTurns g2
  botStatus <- handleBadBots botResults

  DB.writeBotResults botStatus

  commands <- getLegalCommands botResults
  g3 <- applyCommands g2 commands

  g4 <- stepGame g3

  gameLoop g4

-- [(playerid, username, botdir, updatedbot, newbotdir, botstatus)] ->
buildNewBots :: [(Int, T.Text, T.Text, Bool, T.Text, T.Text)] -> IO ([(Int, E.Either T.Text T.Text)])
buildNewBots playerinfo  = mapM buildBot $ filter isNewBots playerinfo
  where
    isNewBots :: (Int, T.Text, T.Text, Bool, T.Text, T.Text) -> Bool
    isNewBots (_, _, _, b, _, _) = b

    buildBot :: (Int, T.Text, T.Text, Bool, T.Text, T.Text) -> IO (Int, E.Either T.Text T.Text)
    buildBot (pid, _, _, _, nbdir, _) = (pid, ) <$> BB.buildBot nbdir

updatePlayerBotHandlers :: Game w h -> readplayers -> IO (Game w h)
updatePlayerBotHandlers = undefined

createNewPlayerStarts :: Game w h -> IO (Game w h)
createNewPlayerStarts = undefined

botTurns :: Game w h -> IO (botresults)
botTurns = undefined

handleBadBots :: botresults -> IO (botStatus)
handleBadBots = undefined

getLegalCommands :: botresults -> IO (commands)
getLegalCommands = undefined

applyCommands :: Game w h -> commands -> IO (Game w h)
applyCommands = undefined

stepGame :: Game w h -> IO (Game w h)
stepGame = undefined

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
