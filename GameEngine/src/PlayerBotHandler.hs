{-# LANGUAGE OverloadedStrings #-}
module PlayerBotHandler
  ( PlayerBotHandler
  , initializePlayer
  , playerTakeTurn
  , updatePlayerBot
  )
    where

import ExternalProcessHandler
import Board
import Player

import qualified Data.Text as T
import qualified Data.Either as E
import qualified Data.Maybe as M
import qualified Data.List as L
import Text.Read
import Types


initializePlayer :: FilePath -> IO ( PlayerBotHandler )
initializePlayer t = PlayerBotHandler . Right <$> createExternalProcess t

-- start new bot process, create new player if needed
-- if there is an error store it in the bothandler
updatePlayerBot :: Game w h -> (PlayerId, FilePath) -> IO (Game w h)
updatePlayerBot g (playerid, newBotPath) = do
  if M.isJust $ L.find (\(x, _) -> pPlayerId  x == playerid) (players g)
  then cleanPlayer $ uneph $ snd $ M.fromJust $ L.find (\(x, _) -> pPlayerId x == playerid) (players g)
  else return ()

  newHandler <- initializePlayer newBotPath
  let newPlayer = M.fromMaybe (Player playerid (-1, -1)) (fst <$> L.find (\(x, _) -> pPlayerId x == playerid) (players g))
  let newPlayers = (newPlayer,  newHandler):(filter (\(x, _) -> pPlayerId x == playerid) (players g))
  return $ g {players = newPlayers}

playerTakeTurn :: Board h w CellInfo -> PlayerBotHandler -> IO (Either T.Text [Command])
playerTakeTurn b pbh = do
  if E.isLeft $ eph pbh
  then return $ Left "Bot handler doesn't exist"
  else do
    let (Right e) = eph pbh
    res <- timedCallandResponse 500 e parsedBoard
    return $ res >>= parsePlayer
  where
    parsedBoard = parseBoard b


parseBoard :: Board h w CellInfo -> T.Text
parseBoard b = T.concat $ map showCell cells
  where
    cells = bCells b
    showCell :: Cell h w CellInfo -> T.Text
    showCell (Cell x y (CellInfo pid)) = T.pack $ (show x) ++ " "
                                               ++ (show y) ++ " "
                                               ++ (show pid) ++ " "

parsePlayer:: T.Text -> (Either T.Text [Command])
parsePlayer t = do
  let bt = T.words t
  il <- mapM textReadEither bt
  split il

  where
    textReadEither :: T.Text -> Either T.Text Int
    textReadEither t1 = translateLeft (\_ -> "NonIntCoordinate") $ readEither (T.unpack t1)

    split :: [Int] -> Either T.Text [Command]
    split [] = Right []
    split (_:[]) = Left "OddNumOfCoordinates"
    split (a:b:r) = ((Flip a b):) <$> split r

translateLeft :: (a -> c) -> Either a b -> Either c b
translateLeft f (Left a) = Left $ f a
translateLeft _ (Right a) = Right a

uneph :: PlayerBotHandler -> ExternalProcessHandler
uneph (PlayerBotHandler (Right e)) = e

