{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Hello (
    HelloMessage (..),
    HelloAPI,
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant

newtype HelloMessage = HelloMessage {msg :: String}
    deriving (Generic)

instance ToJSON HelloMessage

type HelloAPI = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage