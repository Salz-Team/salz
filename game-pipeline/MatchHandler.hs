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
import qualified Data.ByteString.Lazy as LB

-- Utils

data Process = Process
  { processHandle = ProcessHandle
  , stdIn = Maybe Handle
  , stdOut = Maybe Handle
  , stdErr = Maybe Handle
  } deriving Show

spawn :: String -> Process
spawn path = Process ph stdin stdout stderr
  where
    (stdin, stdout, stderr, ph) = createProcess
      (shell path){
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe
      }

logLine line = do
  return ()

dGetStrLn handle = do
  logLine "getting a line"
  line <- hGetLine handle
  logLine $ "got: " ++ line
  return line

dPutStrLn handle line = do
  logLine $ "putting a line: " ++ line
  hPutStrLn handle line
  hFlush handle

-- Args

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

-- Types

data PlayerAction = PlayerAction
  { player :: Int
  , action :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data GameTurn = GameTurn
  { turn_number :: Int
  , actions :: [ PlayerAction ]
  } deriving (Show, Generic, ToJSON, FromJSON)

data GameInfo = GameInfo
  { botPaths :: [String]
  , gameEnginePath :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data GameEngineCommand = Done [Float] | PlayerActions [Int] [LB.ByteString] | Error LB.ByteString deriving (Show)

data GameHistoryLine = GameEngineError LB.ByteString | GameEngineResults [Float] | PlayerActions [(Int, LB.ByteString)] deriving (Show)

instance ToJSON GameHistoryLine where
  toJSON (GameEngineError str) =
    object [ "type" .= "game_error"
           , "error" .= str
           ]
  toJSON (GameEngineResults scores) =
    object [ "type" .= "scores"
           , "scores" .= scores
           ]
  toJSON (PlayerActions actions) =
    object [ "type" .= "player_actions"
           , "turn_number" .= 1
           , "actions" .= actions
           ]

--

botInteract :: Process -> LB.ByteString -> IO LB.ByteString
botInteract bot botin = do
  dPutStrLn (stdIn bot) command
  dGetStrLn (stdOut bot)


handleCommand :: Process -> [Process] -> Command -> IO ()
handleCommand gameEngine bots (Error error) = do
  putStrLn $ encode (GameEngineError error)
  exitWith (ExitFailure 1)
handleCommand gameEngine bots (Done positions) = do
  putStrLn $ encode (GameEngineResults positions)
  exitWith ExitSuccess
handleCommand gameEngine bots (PlayerActions players commands) = do
  botOuts <- mapM (\(p, c) -> botInteract (bots!!p) c) playercmds
  putStrLn $ encode (PlayerActions botOuts)
  mapM (dPutStrLn (stdIn gameEngine)) botOut --todo
  where
    playercmds = zip players commands

gameLoop :: Process -> [Process] -> IO ()
gameLoop gameEngine bots = do
  command_ <- dGetStrLn (stdOut gameEngine)
  let command = commandParser command_
  putStrLn $ "Game:" ++ (show command)
  handleCommand gameEngine bots command
  gameLoop gameEngine bots

main :: IO ()
main = do
  args <- execParser opts
  gameEngine <- spawn (_gameEngine args)
  bots <- mapM spawn (_bots args)
  putStrLn $ "Num Players:" ++ (show (length bots))
  dPutStrLn (getStdIn gameEngine) (show (length bots))
  gameLoop gameEngine bots
