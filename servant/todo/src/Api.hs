{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API) where

import Servant

import Api.File (FileAPI)
import Api.Hello (HelloAPI)
import Api.Marketing (MarketingAPI)
import Api.Person (PersonAPI)
import Api.Position (PositionAPI)
import Api.Storage (StorageAPI)

type API =
    PositionAPI
        :<|> HelloAPI
        :<|> MarketingAPI
        :<|> PersonAPI
        :<|> FileAPI
        :<|> StorageAPI
