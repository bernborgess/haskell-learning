{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Storage (
    StorageAPI,
    storageHandler,
    Message,
    initDB,
) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.SQLite.Simple (
    Only (Only, fromOnly),
    execute,
    execute_,
    query_,
    withConnection,
 )
import Network.HTTP.Client (
    defaultManagerSettings,
    newManager,
 )
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

type Message = String

type StorageAPI =
    ReqBody '[PlainText] Message :> Post '[JSON] NoContent
        :<|> Get '[JSON] [Message]

initDB :: FilePath -> IO ()
initDB dbFile = withConnection dbFile $ \conn ->
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

storageHandler :: FilePath -> (Message -> Handler NoContent) :<|> Handler [Message]
storageHandler dbfile = postMessage :<|> getMessages
  where
    postMessage :: Message -> Handler NoContent
    postMessage msg = do
        liftIO . withConnection dbfile $ \conn ->
            execute conn "INSERT INTO messages VALUES (?)" (Only msg)
        return NoContent

    getMessages :: Handler [Message]
    getMessages = fmap (map fromOnly) . liftIO $
        withConnection dbfile $ \conn ->
            query_ conn "SELECT msg FROM messages"
