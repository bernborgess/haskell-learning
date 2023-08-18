{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Position (
    Position (..),
    PositionAPI,
) where

import Data.Aeson (ToJSON)
import GHC.Generics

import Servant

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    }
    deriving (Generic)

instance ToJSON Position

type PositionAPI = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
