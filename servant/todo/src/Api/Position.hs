{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Position (
    PositionAPI,
    positionHandler,
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

-- ? Export

type PositionAPI = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position

positionHandler :: Int -> Int -> Handler Position
positionHandler x = return . Position x
