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
import Api.File (fileHandler)
import Api.Hello (helloHandler)
import Api.Marketing (marketingHandler)
import Api.Person (personHandler)
import Api.Position (positionHandler)

startApp :: Int -> IO ()
startApp port = withStdoutLogger $ \apacheLogger -> do
  let settings = setPort port $ setLogger apacheLogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server
 where
  api = Proxy :: Proxy API

server :: Server API
server =
  positionHandler
    :<|> helloHandler
    :<|> marketingHandler
    :<|> personHandler
    :<|> fileHandler
