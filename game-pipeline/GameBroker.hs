module Main where

import Options.Applicative
import Control.Monad
import System.Process
import System.IO
import System.Exit
import Data.Maybe
import Data.List
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Parsers

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

opts = info (argparser <**> helper)
         ( fullDesc
        <> progDesc "Run a match")

flushedPutStrLnB handle line = do
  B.hPutStrLn handle (LB.toStrict line)
  hFlush handle

data Process = Process
  { processHandle :: ProcessHandle
  , stdIn :: Handle
  , stdOut :: Handle
  , stdErr :: Maybe Handle
  }

spawn :: String -> IO Process
spawn path = do
  (stdin, stdout, stderr, ph) <- createProcess
    (shell path){
      std_in = CreatePipe,
      std_out = CreatePipe,
      std_err = CreatePipe
    }
  return $ Process ph (fromJust stdin) (fromJust stdout) stderr

liftEither :: Either String a -> IO  a
liftEither (Left errmsg) = error errmsg
liftEither (Right a) = return a

botInteract :: Process -> TicTacToeBoard -> IO TicTacToePlayerResponse
botInteract bot botin = do
  flushedPutStrLnB (stdIn bot) (encode botin)
  fromJust <$> decode <$> LB.fromStrict <$> B.hGetLine (stdOut bot)

handleCommand :: Process -> [Process] -> GameEngineOutMessage TicTacToeBoard -> IO ()
handleCommand gameEngine _ (GameEnd scores) = do
  flushedPutStrLnB stdout $ encode $ (HGameEnd scores :: GameHistoryLine TicTacToePlayerResponse)
  exitWith ExitSuccess
handleCommand gameEngine bots (DebugMessage msg) = do
  flushedPutStrLnB stdout $ encode $ (HDebug msg :: GameHistoryLine TicTacToePlayerResponse)
  gameLoop gameEngine bots
handleCommand gameEngine bots (PlayerTurn playercmds) = do
  botOuts <- mapM (\(p, c) -> botInteract (bots!!p) c) playercmds
  flushedPutStrLnB stdout $ encode $ HPlayerResponses (map (\(x, (p, _)) -> (p, x, True, "", "")) (zip botOuts playercmds))
  flushedPutStrLnB (stdIn gameEngine) $ encode $ PlayerResponses (map (\(x, (p, _)) -> (p, True, x)) (zip botOuts playercmds))
  gameLoop gameEngine bots

gameLoop :: Process -> [Process] -> IO ()
gameLoop gameEngine bots = do
  imsg <- liftEither =<< eitherDecode <$> LB.fromStrict <$> B.hGetLine (stdOut gameEngine)
  handleCommand gameEngine bots imsg

main :: IO ()
main = do
  args <- execParser opts
  gameEngine <- spawn (_gameEngine args)
  bots <- mapM spawn (_bots args)
  flushedPutStrLnB stdout $ encode $ (HGameStart (length bots) "" :: GameHistoryLine TicTacToePlayerResponse)
  flushedPutStrLnB (stdIn gameEngine) $ encode $ (GameStart (length bots) :: GameEngineInMessage TicTacToeBoard)
  gameLoop gameEngine bots
