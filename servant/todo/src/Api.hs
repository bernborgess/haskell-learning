{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api (
    User (..),
    API,
) where

import Data.Aeson
import Data.Aeson.TH
import Servant

data User = User
    { userId :: Int
    , userFirstName :: String
    , userLastName :: String
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]