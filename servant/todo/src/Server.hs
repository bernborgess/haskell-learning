{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Server (
  startApp,
  app,
) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Prelude.Compat
import Prelude ()

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
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
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

import Api (
  API,
  ClientInfo (..),
  Email (..),
  HelloMessage (..),
  Position (..),
  User (..),
  emailForClient,
 )
import Data.Time (fromGregorian)

startApp :: Int -> IO ()
startApp port = run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  position
    :<|> hello
    :<|> marketing
 where
  position :: Int -> Int -> Handler Position
  position x = return . Position x

  hello :: Maybe String -> Handler HelloMessage
  hello =
    return
      . HelloMessage
      . \case
        Nothing -> "Hello, anonymous coward"
        Just n -> "Hello, " ++ n

  marketing :: ClientInfo -> Handler Email
  marketing = return . emailForClient

users :: [User]
users = [isaac, albert]

isaac :: User
isaac = User "Isaac Newton" 23 "isaac_newton@apple.com" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 44 "albert@gmail.com" (fromGregorian 1905 12 1)