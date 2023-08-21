{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Storage (
    StorageAPI,
    storageHandler,
    Message,
    initDB,
) where

import Control.Monad.IO.Class
import Database.SQLite.Simple (
    Only (..),
    execute,
    execute_,
    query_,
    withConnection,
 )

import System.Environment (getEnv)

import Servant

type Message = String

type StorageAPI =
    ReqBody '[PlainText] Message :> Post '[JSON] NoContent
        :<|> Get '[JSON] [Message]

initDB :: FilePath -> IO ()
initDB dbFile = withConnection dbFile $ \conn ->
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

postMessage :: Message -> Handler NoContent
postMessage msg = do
    dbfile <- liftIO (getEnv "DATABASE_FILE")
    liftIO . withConnection dbfile $ \conn ->
        execute conn "INSERT INTO messages VALUES (?)" (Only msg)
    return NoContent

getMessages :: Handler [Message]
getMessages = do
    dbfile <- liftIO (getEnv "DATABASE_FILE")
    fmap (map fromOnly) . liftIO $
        withConnection dbfile $ \conn ->
            query_ conn "SELECT msg FROM messages"

-- ? Export

storageHandler :: (Message -> Handler NoContent) :<|> Handler [Message]
storageHandler =
    postMessage :<|> getMessages
