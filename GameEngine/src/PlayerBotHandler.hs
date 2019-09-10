module PlayerBotHandler
  ( PlayerBotHandler
  , ParseError
  , initializePlayer
  , playerTakeTurn
  )
    where

import ExternalProcessHandler
import Board
import Player

import qualified Data.Text as T
import Data.Either()
import Text.Read
import Types


initializePlayer :: T.Text -> IO (PlayerBotHandler)
initializePlayer t = PlayerBotHandler <$> createExternalProcess t

playerTakeTurn :: Board h w CellInfo -> T.Text -> IO (Either ParseError [Command])
playerTakeTurn b t = do
    pbh <- initializePlayer t
    let e = eph pbh
    res <- timedCallandResponse 500 e parsedBoard
    let transres = translateLeft (Extern) res
    cleanPlayer e
    return $ transres >>= parsePlayer
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

parsePlayer:: T.Text -> (Either ParseError [Command])
parsePlayer t = do
  let bt = T.words t
  il <- mapM textReadEither bt
  split il

  where
    textReadEither :: T.Text -> Either ParseError Int
    textReadEither t1 = translateLeft (\_ -> NonIntCoordinate) $ readEither (T.unpack t1)

    split :: [Int] -> Either ParseError [Command]
    split [] = Right []
    split (_:[]) = Left OddNumOfCoordinates
    split (a:b:r) = ((Flip a b):) <$> split r

translateLeft :: (a -> c) -> Either a b -> Either c b
translateLeft f (Left a) = Left $ f a
translateLeft _ (Right a) = Right a
