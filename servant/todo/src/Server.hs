{-# LANGUAGE DataKinds #-}

module Server (
  startApp,
  app,
  api,
) where

import Prelude.Compat
import Prelude ()

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger

import Servant

import Api (API)
import Api.File (fileHandler)
import Api.Hello (helloHandler)
import Api.Marketing (marketingHandler)
import Api.Person (personHandler)
import Api.Position (positionHandler)
import Api.Storage (storageHandler)

import System.Environment (getEnv)

startApp :: IO ()
startApp = withStdoutLogger appRunner
 where
  appRunner logger = do
    port <- read <$> getEnv "PORT"
    let settings = setPort port $ setLogger logger defaultSettings
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  positionHandler
    :<|> helloHandler
    :<|> marketingHandler
    :<|> personHandler
    :<|> fileHandler
    :<|> storageHandler