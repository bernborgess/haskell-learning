#!/usr/bin/env stack
-- stack --resolver lts-11.5 script --package bytestring --package text --package aeson --package string-conversions --package transformers --package servant-swagger-ui --package wai --package warp --package servant-server --package servant --package servant-swagger-ui --package servant-swagger --package swagger2
{-# LANGUAGE OverloadedStrings,OverloadedStrings,DataKinds,TypeOperators,DeriveGeneric #-}
import Data.String(fromString)
import Data.String.Conversions(cs)
import Control.Monad.IO.Class(liftIO)
import GHC.Generics
import Data.Aeson
-- ------------------------------------------------
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Swagger.UI
import Servant.Swagger
import Data.Swagger(ToSchema)

main :: IO ()
-- main = run 7700 $ serve (Proxy::Proxy API) serverAPI
-- 访问 http://localhost:7700/swagger-ui/ 查看文档
main = run 7700 $ serve (Proxy::Proxy APIWithSwagger) serverAPIWithSwagger

type API = Get '[JSON] (String,String)
  :<|> "cat" :> Capture "name" String :> Get '[JSON] String
  :<|> "enum" :> Get '[JSON] MyData

data MyData = D1 [String] | D2 Int | D3 (Maybe Int) deriving (Show,Eq,Generic)

instance FromJSON MyData
instance ToJSON MyData
instance ToSchema MyData

type APIWithSwagger = SwaggerSchemaUI "swagger-ui" "swagger.json"
  :<|> API

getJSON = do
  liftIO $ putStrLn "getJSON"
  pure ("a","b")

getJSONWithName name = do
  liftIO $ putStrLn name
  pure "getJSONWithName"

serverAPI :: Server API
serverAPI = getJSON :<|> getJSONWithName :<|> return (D3 $ Just 10)

serverAPIWithSwagger :: Server APIWithSwagger
serverAPIWithSwagger = swaggerSchemaUIServer (toSwagger (Proxy::Proxy API)) :<|> serverAPI
