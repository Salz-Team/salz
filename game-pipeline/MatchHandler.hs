module Main where

import Options.Applicative
import System.Process
import System.IO
import System.Exit
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Types
import Utils

data Args = Args
  { _gameEngine :: String
  , _bots :: [String]
  } deriving Show

argparser :: Parser Args
argparser = Args
  <$> strOption
    ( short 'g'
    <> help "path to the game-engine")
  <*> some (strOption
    (short 'b'
    <> help "path to bots"))

opts :: ParserInfo Args
opts = info (argparser <**> helper)
         ( fullDesc
        <> progDesc "Run a match")

data Process = Process
  { processHandle :: ProcessHandle
  , stdIn :: Handle
  , stdOut :: Handle
  , stdErr :: Maybe Handle
  }

spawn :: String -> IO Process
spawn path = do
  (pstdin, pstdout, pstderr, ph) <- createProcess
    (shell path){
      std_in = CreatePipe,
      std_out = CreatePipe
    }
  return $ Process ph (fromJust pstdin) (fromJust pstdout) pstderr

getGameType :: GameEngineOutMessage Value -> Either String String
getGameType (GameOStart i) = Right i
getGameType _ = Left "Expected game engine message"

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

botInteract :: Process -> Value -> IO Value
botInteract bot botin = do
  flushedPutStrLnB (stdIn bot) (encode botin)
  liftEither =<< eitherDecode <$> LB.fromStrict <$> B.hGetLine (stdOut bot)

handleCommand :: Process -> [Process] -> GameEngineOutMessage Value-> IO ()
handleCommand _ _ (GameEnd scores) = do
  flushedPutStrLnB stdout $ encode $ (HGameEnd scores :: GameHistoryLine Value)
  exitWith ExitSuccess
handleCommand gameEngine bots (DebugMessage msg) = do
  flushedPutStrLnB stdout $ encode $ (HDebug msg :: GameHistoryLine Value)
  gameLoop gameEngine bots
handleCommand gameEngine bots (PlayerTurn playercmds) = do
  botOuts <- mapM (\(p, c) -> botInteract (bots!!p) c) playercmds
  flushedPutStrLnB stdout $ encode $ HPlayerResponses (map (\(x, (p, _)) -> (p, x, True, "", "")) (zip botOuts playercmds))
  flushedPutStrLnB (stdIn gameEngine) $ encode $ PlayerResponses (map (\(x, (p, _)) -> (p, True, x)) (zip botOuts playercmds))
  gameLoop gameEngine bots
handleCommand _ _ (GameOStart _) = do
  exitWith (ExitFailure 1)

gameLoop :: Process -> [Process] -> IO ()
gameLoop gameEngine bots = do
  imsg <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.hGetLine (stdOut gameEngine)
  handleCommand gameEngine bots imsg

main :: IO ()
main = do
  args <- execParser opts
  gameEngine <- spawn (_gameEngine args)
  bots <- mapM spawn (_bots args)
  imsg <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.hGetLine (stdOut gameEngine)
  gameType <- liftEither $ getGameType imsg

  flushedPutStrLnB stdout $ encode $ (HGameStart (length bots) gameType :: GameHistoryLine Value)
  flushedPutStrLnB (stdIn gameEngine) $ encode $ (GameStart (length bots) :: GameEngineInMessage Value)
  gameLoop gameEngine bots
