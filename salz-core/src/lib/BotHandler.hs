{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module BotHandler
  ( Bot(..)
  , takeTurn
  , isCrashed
  , isUnBuilt
  , maybeCommands
  )
    where

import qualified Data.Text as T
import qualified Data.Either as E
import Data.Either.Combinators
import qualified Control.Exception as CE
import Control.DeepSeq
import Data.String
import System.FilePath
import System.Process.Typed
import System.Timeout
import System.IO
import System.Exit
import GHC.IO.Handle
import GHC.IO.Exception
import Data.List
import Control.Monad.Trans.Class
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.IO.Class
import Data.Maybe
import Text.Read (readMaybe)

import qualified Map


--------------------------------------------------------------------------------
-- A bot handler
--------------------------------------------------------------------------------

data Bot = Crashed { playerId :: Int
                   , errorLog :: String
                   , memory :: String
                   , errorMsg :: String
                   }
          | NewBot { playerId :: Int
                   , filePath :: FilePath
                   , startingLocation :: (Int, Int)
                   }
          | Bot    { playerId :: Int
                   , filePath :: FilePath
                   , memory :: String
                   , errorLog :: String
                   , commands :: [(Int, Int)]
                   , mapReq :: Maybe ((Int, Int), Int)
                   }
          | UnBuilt { playerId :: Int
                    , filePath :: FilePath
                    } deriving Show

isCrashed :: Bot -> Bool
isCrashed (Crashed _ _ _ _) = True
isCrashed _ = False

isUnBuilt :: Bot -> Bool
isUnBuilt (UnBuilt _ _) = True
isUnBuilt _ = False

--------------------------------------------------------------------------------
-- The bot handler monad
--------------------------------------------------------------------------------

newtype BotT m a = BotT { runBotT :: Bot -> m (Maybe a, Bot) }

instance (Functor m) =>  Functor (BotT m) where
  fmap f (BotT sf) = BotT (fmap (\(a, ns) -> (f <$> a, ns)) . sf)

instance (Monad m) => Applicative (BotT m) where
  pure a = BotT $ \s -> case () of
    _ | isCrashed s -> return (Nothing, s)
      | otherwise   -> return (Just a, s)

  (BotT mf) <*> (BotT ma) = BotT $ \s -> do
      (f, s') <- mf s
      maybe (return (Nothing, s')) (\f' -> fmap (\(a, s'') -> (f' <$> a, s'')) (ma s')) f

instance (Monad m) => Monad (BotT m) where
  return = pure
  (BotT ma) >>= mf = BotT $ \s -> do
    (a, s') <- ma s
    maybe (return (Nothing, s')) (\a' -> runBotT (mf a') s') a

instance (Monad m) => MonadState Bot (BotT m) where
  get = BotT $ \s -> return (Just s, s)
  put s' = BotT $ \s -> case () of
    _ | isCrashed s -> return (Nothing, s)
      | otherwise   -> return (Just (), s')

instance (MonadIO m) => MonadIO (BotT m) where
  liftIO ma = BotT $ \s -> case () of
    _ | isCrashed s -> return (Nothing, s)
      | otherwise   -> do
          a <- liftIO $ CE.try ma
          case a of
            (Left err) -> return (Nothing, handleIOError s err)
            (Right res) -> return (Just res, s)
    where
      handleIOError :: Bot -> IOError -> Bot
      handleIOError (Bot pid _ mem errlog _ _) err = Crashed pid errlog mem (GHC.IO.Exception.ioe_description err)
      handleIOError (NewBot pid _ _) err = Crashed pid "" "" (GHC.IO.Exception.ioe_description err)
      handleIOError a _ = a

killBot :: (Monad m) => BotT m a
killBot = BotT $ \s -> return (Nothing, s)

tryIO :: String -> IO (Maybe a) -> (BotT IO a)
tryIO errmsg ma = do
  a <- liftIO ma
  bot <- get
  case a of
    Nothing -> put (handleError bot errmsg) >> killBot
    Just a' -> return a'
  where
    handleError :: Bot -> String -> Bot
    handleError (Bot pid _ mem errlog _ _) ermsg = Crashed pid errlog mem ermsg
    handleError (NewBot pid _ _) ermsg = Crashed pid "" "" ermsg
    handleError a _ = a


execBot :: Process a b c -> Bot -> BotT IO () -> IO Bot
execBot p b botM = snd <$> runBotT botM b <* stopProcess p


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------


initialize :: Bot -> IO Bot
initialize (Crashed a b c d) = return (Crashed a b c d)
initialize (Bot a b c d e f) = return (Bot a b c d e f)
initialize (UnBuilt a b) = return (UnBuilt a b)
initialize b = withProcessWait botConfig $ \p -> execBot p b $ do
  bot <- get
  liftIO $ hPutStrLn (getStdin p) $ unwords [ "Initialize"
                                            , show (playerId bot)
                                            , show (fst $ startingLocation bot)
                                            , show (snd $ startingLocation bot)
                                            ]
  liftIO $ hFlush (getStdin p)

  response <- tryIO "Timeout while waiting for initalize response." $ timeout 1000000 (hGetLine (getStdout p))
  put $ Bot (playerId bot) (filePath bot) response "" [] Nothing

  return ()
  where
    botConfig = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ setWorkingDir (dropFileName (filePath b))
              $ fromString (filePath b)

takeTurn :: Map.Map -> Bot -> IO Bot
takeTurn _ (Crashed a b c d) = return (Crashed a b c d)
takeTurn map (NewBot a b c) = initialize (NewBot a b c) >>= takeTurn map
takeTurn _ (UnBuilt a b) = return (UnBuilt a b)
takeTurn map_ b = withProcessWait botConfig $ \p -> execBot p b $ do
  bot <- get
  liftIO $ hPutStrLn (getStdin p) (memory b)
  liftIO $ hFlush (getStdin p)
  response <- tryIO "Timeout while waiting for map request." $ timeout 1000000 (hGetLine (getStdout p))
  mapReq <- tryIO "Couldn't parse map request." $ return (readMapRequest response)
  modify (\bot -> bot {mapReq = Just mapReq})
  liftIO $ hPutStrLn (getStdin p) (show (Map.getRegion map_ mapReq))
  liftIO $ hFlush (getStdin p)
  response <- tryIO "Timeout while waiting for commands." $ timeout 1000000 (hGetLine (getStdout p))
  modify (\bot -> bot {commands = readCommands response})
  response <- tryIO "Timeout while waiting for new memory." $ timeout 1000000 (hGetLine (getStdout p))
  modify (\bot -> bot {memory = response})
  response' <- tryIO "Timeout while waiting for stderr to close." $ timeout 1000000 (hGetContents (getStderr p))
  response <- liftIO $ CE.evaluate $ force response'
  modify (\bot -> bot {errorLog = errorLog bot ++ response})


  return ()
  where
    botConfig = setStdin createPipe
              $ setStdout createPipe
              $ setStderr createPipe
              $ setWorkingDir (dropFileName (filePath b))
              $ fromString (filePath b)




--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

maybeCommands :: Bot -> Maybe [(Map.Coord, Int)]
maybeCommands (Bot pid _ _ _ cmds _) = Just $ map (\a -> (Map.toCoord a, pid)) cmds
maybeCommands  _ = Nothing

readCommands :: String -> [(Int, Int)]
readCommands str = take 3 $ duify intList
  where
    intList = mapMaybe readMaybe $ words str
    duify (a:b:rst) = (a, b) : (duify rst)
    duify _ = []

readMapRequest :: String -> Maybe ((Int, Int), Int)
readMapRequest str = threeify intList
  where
    intList = mapMaybe readMaybe $ words str
    threeify [a,b,c] = Just ((a, b), c)
    threeify _ = Nothing
