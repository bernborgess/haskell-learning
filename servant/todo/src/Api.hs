{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api (
    API,
    ClientInfo (..),
    Email (..),
    HelloMessage (..),
    Position (..),
    User (..),
    emailForClient,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.List
import Data.Time.Calendar
import GHC.Generics
import Servant

-- ? User
data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    }
    deriving (Eq, Show, Generic)

instance ToJSON User

type UserAPI =
    "users" :> Get '[JSON] [User]
        :<|> "albert" :> Get '[JSON] User
        :<|> "isaac" :> Get '[JSON] User

-- ? Position
data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    }
    deriving (Generic)

instance ToJSON Position

-- ? HelloMessage
newtype HelloMessage = HelloMessage {msg :: String}
    deriving (Generic)

instance ToJSON HelloMessage

-- ? ClientInfo
data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    }
    deriving (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

-- ? Email
data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    }
    deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "hr@teslamotors.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
        "Hi " ++ clientName c ++ ",\n\n"
            ++ "Since you've recently turned "
            ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"

type API =
    "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
        :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
        :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
