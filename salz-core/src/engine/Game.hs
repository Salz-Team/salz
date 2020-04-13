{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds #-}

module Game ( startGameEngine
            ) where
    
import qualified Player as P
import qualified BotHandler as BH
import qualified Step as S
import EngineOptions
import qualified BotBuilder as BB
import qualified Map as Map
import qualified Database as DB

import qualified Data.Text as T
import qualified Control.Concurrent as CC
import qualified System.IO as SO

import GHC.TypeLits hiding (Mod)
import qualified Data.Either as E
import Data.Maybe
import Data.List

startGameEngine :: EngineCmdLineArgs -> IO ()
startGameEngine arg = case arg of
  ServerArgs -> do
    dbstring <- connectionString
    SO.hSetBuffering SO.stdout SO.LineBuffering
    serverGameLoop (Map.M []) (-1) [] dbstring
  LocalArgs dbFilePath turnMax buildPaths -> do
    let bots = map (\(pid, tarPath) -> BH.UnBuilt pid tarPath) $ zip [1..] buildPaths
    bots' <- mapM BB.buildBot bots
  
    localGameLoop (Map.M []) (-1) turnMax bots' dbFilePath


serverGameLoop :: Map.Map -> Int -> [BH.Bot] -> T.Text -> IO ()
serverGameLoop map_ turnm bots dbConnectionString = do
  putStrLn ""
  putStrLn "New turn"
  putStrLn ""

  let turn = turnm+1

  putStrLn "Updating Players"
  buildCmds <- DB.getBuildCmds (Left dbConnectionString)
  builtBots <- mapM BB.buildBot buildCmds

  let bots' = unionBy (\a b -> BH.playerId a == BH.playerId b) builtBots bots
  let map' = createSpawns map_ bots'

  putStrLn "Run Bots"
  bots'' <- mapM (BH.takeTurn map') bots'
  let cmds  = P.filterLegalCommands map' $ concat $ catMaybes $ map BH.maybeCommands bots''
  let map'' = P.applyCommands map' cmds

  putStrLn "Step Game"
  let map''' = S.step map''

  putStrLn "Save Status"
  DB.saveMoves (Left dbConnectionString) turn cmds
  if (turn `mod` 100 == 0)
  then DB.saveSnapshot (Left dbConnectionString) turn map'''
  else return ()
  DB.savePlayersStatus (Left dbConnectionString) bots''

  putStrLn "Wait"
  CC.threadDelay 1000000
  serverGameLoop map''' turn bots'' dbConnectionString

localGameLoop :: Map.Map -> Int -> Int -> [BH.Bot] -> FilePath -> IO ()
localGameLoop map_ pturn turnMax bots dbFilePath = do
  let turn = pturn+1
  putStrLn $ "Turn " ++ (show turn)


  putStrLn "Run Bots"
  let map' = createSpawns map_ bots
  bots' <- mapM (BH.takeTurn map') bots
  let cmds  = P.filterLegalCommands map' $ concat $ catMaybes $ map BH.maybeCommands bots'
  let map'' = P.applyCommands map' cmds


  putStrLn "Step Game"
  let map''' = S.step map''

  putStrLn "Save Status"
  DB.saveMoves (Right dbFilePath) turn cmds
  if (turn `mod` 100 == 0)
  then DB.saveSnapshot (Right dbFilePath) turn map'''
  else return ()
  DB.savePlayersStatus (Right dbFilePath) bots'


  if (turn >= turnMax)
  then return ()
  else localGameLoop map''' turn turnMax bots' dbFilePath


createSpawns :: Map.Map -> [BH.Bot] -> Map.Map
createSpawns (Map.M mlst) bots = Map.M $ foldl fillStartingLocation mlst nBots
  where
    nBots = filter (\bot -> 0 == length (P.getPlayerCells (Map.M mlst) (BH.playerId bot))) bots

    fillStartingLocation :: [(Map.Coord, Int)] -> BH.Bot -> [(Map.Coord, Int)]
    fillStartingLocation mlst' bot
      | BH.isCrashed bot = mlst'
      | BH.isUnBuilt bot = mlst'
      | otherwise        = (Map.toCoord (getStartLoc $ BH.playerId bot), BH.playerId bot):mlst'

getStartLoc :: Int -> (Int, Int)
getStartLoc seed = (seed * 5790153, seed * 57281)
