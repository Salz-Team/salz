module Main where

import Options.Applicative
import Control.Monad
import System.Process
import System.IO
import System.Exit
import Data.Maybe
import Data.List

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

spawn path =
  createProcess
    (shell path){
      std_in = CreatePipe,
      std_out = CreatePipe
    }

getStdIn (x, _, _, _) = fromJust x
getStdOut (_, x, _, _) = fromJust x

logLine line = do
  return ()

-- spawn game engine
-- spawn bots
-- game eng interface:
--  handler -> game eng: <# players>
--  game eng -> handler: <player #> + <timeout>
--  game eng -> handler: <player # stdin>
--  handler -> bot #: <player # stdin>
--  bot # -> handler: <player # stdout>
--  hanlder -> game eng: <player # stdout>

dGetStrLn handle = do
  logLine "getting a line"
  line <- hGetLine handle
  logLine $ "got: " ++ line
  return line

dPutStrLn handle line = do
  logLine $ "putting a line: " ++ line
  hPutStrLn handle line
  hFlush handle

data Command = Done [Float] | Player Int Int | Error String deriving (Show)

commandParser command
  | "Finished" `isPrefixOf` command = Done (map read $ tail $ words command)
  | "Player" `isPrefixOf` command = Player (read ((words command)!!1)) (read (last (words command)))
  | "Error" `isPrefixOf` command = Error command

handleCommand :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)] -> Command -> IO ()
handleCommand gameEngine bots (Error error) = do
  putStrLn error
  exitWith (ExitFailure 1)

handleCommand gameEngine bots (Done positions) = do
  putStrLn (intercalate " " (map show positions))

handleCommand gameEngine bots (Player player len) = do
  gameOut <- replicateM len (dGetStrLn (getStdOut gameEngine))
  mapM (dPutStrLn (getStdIn (bots!!player))) gameOut
  mapM (\x -> putStrLn ("Game->Bot:" ++ x)) gameOut
  botLen <- dGetStrLn (getStdOut (bots!!player))
  botOut <- replicateM (read botLen) (dGetStrLn (getStdOut (bots!!player)))
  mapM (\x -> putStrLn ("Bot->Game:" ++ x)) botOut
  mapM (dPutStrLn (getStdIn gameEngine)) botOut
  gameLoop gameEngine bots

gameLoop :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)] -> IO ()
gameLoop gameEngine bots = do
  command_ <- dGetStrLn (getStdOut gameEngine)
  let command = commandParser command_
  putStrLn $ "Game:" ++ (show command)
  handleCommand gameEngine bots command

main :: IO ()
main = do
  args <- execParser opts
  gameEngine <- spawn (_gameEngine args)
  bots <- mapM spawn (_bots args)
  putStrLn $ "Num Players:" ++ (show (length bots))
  dPutStrLn (getStdIn gameEngine) (show (length bots))
  gameLoop gameEngine bots
