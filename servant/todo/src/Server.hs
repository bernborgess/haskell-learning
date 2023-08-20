{-# LANGUAGE DataKinds #-}

module Server (
  startApp,
  app,
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

startApp :: Int -> IO ()
startApp port = withStdoutLogger $ \logger -> do
  let settings = setPort port $ setLogger logger defaultSettings
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
