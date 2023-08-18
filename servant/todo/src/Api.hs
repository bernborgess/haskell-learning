{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api (API) where

import Api.Hello (HelloAPI)
import Api.Marketing (MarketingAPI)
import Api.Position (PositionAPI)
import Servant

type API = PositionAPI :<|> HelloAPI :<|> MarketingAPI
