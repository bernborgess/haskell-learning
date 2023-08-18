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
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

import Api (API)
import Api.Hello (HelloMessage (..))
import Api.Marketing (ClientInfo, Email, emailForClient)
import Api.Position (Position (..))

startApp :: Int -> IO ()
startApp port =
  withStdoutLogger $ \apacheLogger -> do
    let settings = setPort port $ setLogger apacheLogger defaultSettings
    runSettings settings app

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