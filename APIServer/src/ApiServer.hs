{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiServer where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Servant.Multipart

type ObaApi = GameApi :<|> UserApi

type GameApi = "getHistory" :> Get '[JSON] [GameTurn]

type UserApi = "signUp" :> ReqBody '[JSON] User :> Post '[JSON] User
          :<|> "getUsers" :> Get '[JSON] [User]
          :<|> "quit" :> ReqBody '[JSON] User :> Post '[JSON] UserId
          :<|> "uploadBot" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Int

data GameTurn = GameTurn
  { turn :: Int
  } deriving (Generic, Show, Eq)

instance ToJSON GameTurn

type UserId = Int

data User = User
  { id :: UserId
  } deriving (Generic, Show, Eq)

instance ToJSON User
instance FromJSON User

gameTurns =
    [GameTurn 1, GameTurn 2, GameTurn 3]

users = [User 1, User 2, User 3]


obaServer :: Server ObaApi
obaServer = return gameTurns
       :<|> handleSignUp
       :<|> return users
       :<|> handleQuit
       :<|> handleBotUpload
  where
    handleBotUpload multiData = return 0

    handleSignUp :: User -> Handler User
    handleSignUp u = return u

    handleQuit :: User -> Handler UserId
    handleQuit u = return (ApiServer.id u)

obaApi :: Proxy ObaApi
obaApi = Proxy

app1 :: Application
app1 = serve obaApi obaServer


apiServer :: IO ()
apiServer = run 8081 app1

