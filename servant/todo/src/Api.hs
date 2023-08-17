{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api (
    User (..),
    API,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Time.Calendar
import GHC.Generics
import Servant

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    }
    deriving (Eq, Show, Generic)

instance ToJSON User

type API =
    "users" :> Get '[JSON] [User]
        :<|> "albert" :> Get '[JSON] User
        :<|> "isaac" :> Get '[JSON] User