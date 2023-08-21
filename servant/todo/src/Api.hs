{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API) where

import Servant
import Servant.Swagger

import Api.File (FileAPI)
import Api.Hello (HelloAPI)
import Api.Marketing (MarketingAPI)
import Api.Person (PersonAPI)
import Api.Position (PositionAPI)
import Api.Storage (StorageAPI)

-- type SwaggerAPI = "swagger" :> Get '[JSON] Swagger

type API =
    "position" :> PositionAPI
        :<|> "hello" :> HelloAPI
        :<|> "marketing" :> MarketingAPI
        :<|> "persons" :> PersonAPI
        :<|> "files" :> FileAPI
        :<|> "database" :> StorageAPI

-- :<|> SwaggerAPI
