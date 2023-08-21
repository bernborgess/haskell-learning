{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Position (
    PositionAPI,
    positionHandler,
) where

-- import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Typeable (Typeable)
import GHC.Generics
import Servant
import Servant.Swagger

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Data.Swagger
import Data.Typeable (Typeable)
import Servant
import Servant.Swagger

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    }
    deriving (Show, Generic, Typeable)

instance ToJSON Position

-- instance FromJSON Position

-- instance ToSchema Position where
--     declareNamedSchema proxy =
--         genericDeclareNamedSchema defaultSchemaOptions proxy
--             & mapped.schema . description

-- ? Export

type PositionAPI =
    Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position

-- :<|> "docs" :> Get '[JSON] Swagger

positionHandler :: Int -> Int -> Handler Position
positionHandler x = return . Position x

-- swaggerHandler ::
