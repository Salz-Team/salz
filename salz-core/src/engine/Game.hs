{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds #-}

module Game ( startServerGameEngine
            , startLocalGameEngine
            ) where
    
import Player
import qualified BotHandler as BH
import Data.Modular
import Step
import Types
import qualified BotBuilder as BB
import qualified Board as B
import qualified Database as DB
import qualified Data.Text as T
import qualified Control.Concurrent as CC
import qualified System.IO as SO

import GHC.TypeLits hiding (Mod)
import qualified Data.Either as E

startServerGameEngine :: T.Text -> IO ()
startServerGameEngine dbstring = do
  SO.hSetBuffering SO.stdout SO.LineBuffering
  serverGameLoop (Board [] :: Board 100 100 CellInfo) 0 [] dbstring

serverGameLoop :: (KnownNat w, KnownNat h) => Board w h CellInfo -> Int -> [Player] -> T.Text -> IO ()
serverGameLoop board turnm players dbConnectionString = do
  putStrLn ""
  putStrLn "New turn"
  putStrLn ""

  let turn = turnm+1

  putStrLn "Updating Players"
  buildCmds <- DB.getBuildCmds (Left dbConnectionString)
  botDirs <- mapMSecond BB.buildBot buildCmds
  newHandlers <- mapMSecond_ (\pid -> E.either (return . BotHandler . Left) (BH.startBot pid)) botDirs :: IO [(PlayerId, BotHandler)]

  let players1 = updatePlayers newHandlers players
  let board1 = createSpawns board players1

  putStrLn "Run Bots"
  botCmds <- mapM (BH.botTurn board1) players1
  let players2 = map fst botCmds
  let board2 = applyCommands board1 botCmds

  putStrLn "Step Game"
  let board3 = step board2

  putStrLn "Save Status"
  DB.saveBoard (Left dbConnectionString) turn board3
  DB.savePlayers (Left dbConnectionString) players2

  putStrLn "Wait"
  CC.threadDelay 1000000
  serverGameLoop board3 turn players2 dbConnectionString

startLocalGameEngine :: [String] -> IO ()
startLocalGameEngine args = do
  let dbFilePath = args!!0
  let turnMax = read (args!!1)
  let buildCmds = zip [1..] (drop 2 args)

  botDirs <- mapMSecond BB.buildBot buildCmds
  newHandlers <- mapMSecond_ (\pid -> E.either (return . BotHandler . Left) (BH.startBot pid)) botDirs :: IO [(PlayerId, BotHandler)]
  let players = updatePlayers newHandlers []
  let board = createSpawns (Board [] :: Board 100 100 CellInfo) players

  localGameLoop board 0 turnMax players dbFilePath

localGameLoop :: (KnownNat w, KnownNat h) => Board w h CellInfo -> Int -> Int -> [Player] -> FilePath -> IO ()
localGameLoop board pturn turnMax players dbFilePath = do
  let turn = pturn+1
  putStrLn $ "Turn " ++ (show turn)

  print board

  putStrLn "Run Bots"
  botCmds <- mapM (BH.botTurn board) players
  let players1 = map fst botCmds
  let board1 = applyCommands board botCmds
  print $ map snd botCmds
  print board1

  putStrLn "Step Game"
  let board2 = step board1
  print  board2

  putStrLn "Save Status"
  DB.saveBoard (Right dbFilePath) turn board2

  putStrLn "Bot Status"
  _ <- mapM (\x -> putStrLn $ T.unpack (E.fromLeft "All good" x)) $ map (eph . pBotHandler) players1

  if (turn >= turnMax)
  then return ()
  else localGameLoop board2 turn turnMax players1 dbFilePath


createSpawns :: (KnownNat w, KnownNat h) => Board w h CellInfo -> [Player] -> Board w h CellInfo
createSpawns board players = foldl fillStartingLocation board nplayers
  where
    nplayers :: [PlayerId]
    nplayers = filter (\pid -> 0 == length (getPlayerCells board pid)) $ map pPlayerId players
    fillStartingLocation :: (KnownNat w, KnownNat h) =>
                              Board w h CellInfo -> PlayerId -> Board w h CellInfo
    fillStartingLocation board1 pid = B.toggleCell board1 (mkCell pid)
    mkCell pid = Cell (toMod $ fst $ getStartLoc pid) (toMod $ snd $ getStartLoc pid) (CellInfo pid)

getStartLoc :: Int -> (Int, Int)
getStartLoc seed = (seed * 5790153, seed * 57281)

updatePlayers :: [(PlayerId, BotHandler)] -> [Player] -> [Player]
updatePlayers newBotHandlers oldPlayers = foldl updatePlayer oldPlayers newBotHandlers
  where
    updatePlayer players (pid, botHandler) = if elem pid (map pPlayerId players)
      then map (\(Player pid1 pbd) -> if pid1 == pid
                                      then (Player pid botHandler)
                                      else (Player pid1 pbd)) players
      else (Player pid botHandler):players

mapMSecond :: Monad m => (a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSecond f = mapM (\(c, a) -> (\x -> (c, x)) <$> f a)

mapMSecond_ :: Monad m => (c -> a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSecond_ f = mapM (\(c, a) -> (\x -> (c, x)) <$> f c a)
