{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API) where

import Api.File (FileAPI)
import Api.Hello (HelloAPI)
import Api.Marketing (MarketingAPI)
import Api.Person (PersonAPI)
import Api.Position (PositionAPI)
import Servant

type API =
    PositionAPI
        :<|> HelloAPI
        :<|> MarketingAPI
        :<|> PersonAPI
        :<|> FileAPI
