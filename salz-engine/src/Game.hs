{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds #-}

module Game where
    
import Player
import qualified PlayerBotHandler as PBH
import Board
import Step
import Types
import qualified BotBuilder as BB
import qualified System.FilePath as FP
import qualified Database as DB
import qualified Data.Text as T
import qualified Control.Monad as CM
import qualified Control.Concurrent as CC
import qualified System.IO as SO

import GHC.TypeLits hiding (Mod)
import Data.Maybe
import qualified Data.Either as E
import Data.Modular


startServerGameEngine :: T.Text -> IO ()
startServerGameEngine dbstring = do
  let g = Game (Board []) [] 1 dbstring "/tmp/" :: Game 100 100
  SO.hSetBuffering SO.stdout SO.LineBuffering
  serverGameLoop g

serverGameLoop :: (KnownNat w, KnownNat h) => Board w h -> [Player] -> T.Text -> IO ()
serverGameLoop board players dbConnectionString = do
  putStrLn ""
  putStrLn "New turn"
  putStrLn ""

  putStrLn "Updating Players"
  buildCmds <- DB.getBuildCmds dbConnectionString
  botDirs <- mapSecond B.buildBot buildCmds
  newHandlers <- mapSecond (startBot . <$>) botDirs
  let players1 = updatePlayers newHandlers
  let board1 = createSpawns board players1

  putStrLn "Run Bots"
  (players2, botCmds) <- botTurns board1 players1
  let cmds = getLegalCommands board1 botCmds

  putStrLn "Step Game"
  board2 <- foldl applyCommand board1 cmds
  board3 <- step board2

  putStrLn "Save Status"
  DB.saveBoard dbConnectionString board3
  DB.savePlayers dbConnectionString players2

  putStrLn "Wait"
  CC.threadDelay 1000000
  serverGameLoop board3 players2 dbConnectionString



createSpawns :: (KnownNat w, KnownNat h) => Board w h -> [Player] -> Board w h
createSpawns = undefined
-- createSpawns g = return $ g {board = nboard, players = initializedplayers ++ nplayers}
--   where
--     uninitializedplayers = filter (\((p), _) -> pPlayerSource p == (-1, -1)) (players g)
--     initializedplayers = filter (\((p), _) -> pPlayerSource p /= (-1, -1)) (players g)
--     nplayers = map (\(p, e) -> (p {pPlayerSource = getStartLoc (pPlayerId p)}, e)) uninitializedplayers
--     nboard = foldl fillStartingLocation (board g) $ map fst nplayers

getStartLoc :: Int -> (Int, Int)
getStartLoc seed = (seed * 5790153, seed * 57281)

updatePlayers :: [(Playerid, Either T.Text PlayerBotHandler)] -> [Player] -> [Player]
updatePlayers = undefined

botTurns :: Board w h CellInfo -> [Player] -> IO [(Player, [Command])]
botTurns = undefined
