{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds #-}

module Game ( startServerGameEngine
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
  buildCmds <- DB.getBuildCmds dbConnectionString
  botDirs <- mapMSecond BB.buildBot buildCmds
  newHandlers <- mapMSecond (E.either (return . BotHandler . Left) BH.startBot) botDirs
  let players1 = updatePlayers newHandlers players
  let board1 = createSpawns board players1

  putStrLn "Run Bots"
  botCmds <- mapM (BH.botTurn board1) players1
  let players2 = map fst botCmds
  let board2 = applyCommands board1 botCmds

  putStrLn "Step Game"
  let board3 = step board2

  putStrLn "Save Status"
  DB.saveBoard dbConnectionString turn board3
  DB.savePlayers dbConnectionString players2

  putStrLn "Wait"
  CC.threadDelay 1000000
  serverGameLoop board3 turn players2 dbConnectionString

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
updatePlayers = undefined

mapMSecond :: Monad m => (a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSecond f = mapM (\(c, a) -> (\x -> (c, x)) <$> f a)
