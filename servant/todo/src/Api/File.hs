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
import Data.Char (isAlpha)
import GHC.Generics (Generic)
import Servant
import System.Directory (doesFileExist)

type FileAPI = Capture "filename" String :> Get '[JSON] FileContent

newtype FileContent = FileContent
    {content :: String}
    deriving (Generic)

instance ToJSON FileContent

sanitize :: [Char] -> [Char]
sanitize = filter (\c -> isAlpha c || c == '.')

fileHandler :: String -> Handler FileContent
fileHandler rawFilename = do
    let filename = sanitize rawFilename
    let path = "files/" ++ filename
    exists <- liftIO (doesFileExist path)
    if exists
        then do
            fileContent <- liftIO (readFile path)
            return (FileContent fileContent)
        else throwError custom404Err
  where
    custom404Err = err404{errBody = "file not found"}
