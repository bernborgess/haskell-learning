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
import Storage (StorageAPI, storageHandler)

startApp :: Int -> FilePath -> IO ()
startApp port dbfile = withStdoutLogger $ \logger -> do
  let settings = setPort port $ setLogger logger defaultSettings
  runSettings settings $ app dbfile

app :: FilePath -> Application
app = serve api . server

api :: Proxy StorageAPI
api = Proxy

server :: FilePath -> Server StorageAPI
server = storageHandler

-- positionHandler
--   :<|> helloHandler
--   :<|> marketingHandler
--   :<|> personHandler
--   :<|> fileHandler
--   :<|>
