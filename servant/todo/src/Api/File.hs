{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.File (
    FileAPI,
    fileHandler,
) where

import Control.Monad.Cont
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant (
    Get,
    Handler,
    JSON,
    ServerError (errBody),
    err404,
    throwError,
    (:>),
 )
import System.Directory (doesFileExist)

type FileAPI = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
    {content :: String}
    deriving (Generic)

instance ToJSON FileContent

fileHandler :: Handler FileContent
fileHandler = do
    exists <- liftIO (doesFileExist filename)
    if exists
        then do
            fileContent <- liftIO (readFile filename)
            return (FileContent fileContent)
        else throwError custom404Err
  where
    filename = "myfile.txt"
    custom404Err = err404{errBody = "myfile.txt just moved"}
