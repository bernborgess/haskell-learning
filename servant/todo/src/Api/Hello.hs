{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Api.Hello (
    HelloAPI,
    helloHandler,
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant

newtype HelloMessage = HelloMessage {msg :: String}
    deriving (Generic)

instance ToJSON HelloMessage

type HelloAPI = QueryParam "name" String :> Get '[JSON] HelloMessage

helloHandler :: Maybe String -> Handler HelloMessage
helloHandler =
    return
        . HelloMessage
        . \case
            Nothing -> "Hello, anonymous coward"
            Just n -> "Hello, " ++ n
