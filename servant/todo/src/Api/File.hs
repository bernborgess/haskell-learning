{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.File (
    FileAPI,
    fileHandler,
) where

import Control.Monad.Cont
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant

type FileAPI = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
    {content :: String}
    deriving (Generic)

instance ToJSON FileContent

fileHandler :: Handler FileContent
fileHandler = do
    fileContent <- liftIO (readFile "myfile.txt")
    -- let fileContent = "content"
    -- liftIO (writeFile "somefile.txt" fileContent)

    return (FileContent fileContent)
