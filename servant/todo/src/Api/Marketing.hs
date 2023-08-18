{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Marketing (
    ClientInfo (..),
    Email (..),
    emailForClient,
    MarketingAPI,
) where

import Data.Aeson
import Data.List (intercalate)
import GHC.Generics (Generic)
import Servant

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    }
    deriving (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

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

type MarketingAPI = "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email