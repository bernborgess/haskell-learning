{-# LANGUAGE DataKinds #-}

module Server (
  startApp,
  app,
) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api (API, User (User))

startApp :: Int -> IO ()
startApp port = run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users =
  [ User "Isaac Newton" 23 "isaac_newton@apple.com"
  , User "Albert Einstein" 44 "albert@gmail.com" ]