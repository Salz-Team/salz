{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds #-}

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
import qualified Control.Concurrent as CC
import qualified System.IO as SO

import GHC.TypeLits hiding (Mod)
import Data.Maybe
import qualified Data.Either as E
import Data.Modular


startGameEngine :: T.Text -> IO ()
startGameEngine dbstring = do
  let g = Game (Board []) [] 1 dbstring "/tmp/" :: Game 100 100
  gameLoop g

gameLoop :: (KnownNat w, KnownNat h) => Game w h -> IO ()
gameLoop g = do
  SO.hSetBuffering SO.stdout SO.LineBuffering
  putStrLn ""
  putStrLn $ "New turn" ++ (show (turn g))
  putStrLn ""


  putStrLn "Reading Player info"

  dbplayerinfo <- filterMaybies <$> DB.readPlayers (dbconnstring g)
  putStrLn $ "Build new bots"
  buildstatus <- buildNewBots (botDir g) dbplayerinfo
  putStrLn $ "Write build results"
  DB.writeBuildResults (dbconnstring g) buildstatus

  putStrLn $ "Update new Player bots"
  -- Start new players bots, replace old bots with new bots
  g1 <- updatePlayerBotHandlers g buildstatus
  g2 <- createNewPlayerStarts g1

  putStrLn $ "Number of active cell is " ++ (show (length (bCells (board g2))))

  putStrLn ""
  putStrLn $ "Players:"
  print $ map fst (players g2)
  botResults <- botTurns g2

  print botResults

  DB.writeBotResults (dbconnstring g) botResults

  let commands = getLegalCommands g2  botResults
  let g3 = applyCommands g2 commands
  let g4 = stepGame g3

  DB.saveGame (dbconnstring g) g4

  CC.threadDelay 1000000
  gameLoop g4

filterMaybies :: [(Int, Maybe T.Text, Maybe FilePath, Maybe Bool, Maybe FilePath, Maybe T.Text)] -> [(Int, T.Text, FilePath, Bool, FilePath, T.Text)]
filterMaybies s = map fromJust $ filter (isJust) $ map help s
  where
    help (a, _, _, Just d, Just e, _) = Just (a, "", "", d, e, "")
    help _ = Nothing

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


createNewPlayerStarts :: (KnownNat w, KnownNat h) => Game w h -> IO (Game w h)
createNewPlayerStarts g = return $ g {board = nboard, players = initializedplayers ++ nplayers}
  where
    uninitializedplayers = filter (\((p), _) -> pPlayerSource p == (-1, -1)) (players g)
    initializedplayers = filter (\((p), _) -> pPlayerSource p /= (-1, -1)) (players g)
    nplayers = map (\(p, e) -> (p {pPlayerSource = getStartLoc (pPlayerId p)}, e)) uninitializedplayers
    nboard = foldl fillStartingLocation (board g) $ map fst nplayers

getStartLoc :: Int -> (Int, Int)
getStartLoc seed = (seed * 5790153, seed * 57281)


botTurns :: Game w h -> IO ([(Int, E.Either T.Text [Command])])
botTurns g = mapM (\(p, e) -> (pify $ pPlayerId p) <$> PBH.playerTakeTurn b e)  pl
  where
    b = board g
    pl = players g
    pify p x = (p, x)

getLegalCommands :: (KnownNat w, KnownNat h) => Game w h -> [(Int, E.Either T.Text [Command])] -> [(Int, Command)]
getLegalCommands g rsp = filter pp collapsed
  where
    pp (pid, cmd) = isCommandValid (board g) pid cmd
    nonbrokenbots = map (\(a, b) -> (a, E.fromRight [] b)) $ filter (\(_, x) -> not $E.isLeft x) rsp
    collapsed = concat $ map (\(pid, lst) -> map (\a -> (pid, a)) lst) nonbrokenbots

applyCommands :: (KnownNat w, KnownNat h) => Game w h -> [(Int, Command)] -> Game w h
applyCommands g cl = g {board = nboard}
  where
    nboard = foldl (applyCommand (board g)) (board g) $ map (\(a, b) -> (a, [b])) cl

stepGame :: (KnownNat w, KnownNat h) => Game w h -> Game w h
stepGame g = g {board = step (board g), turn = (turn g) + 1}


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
