module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant (NoContent)
import Servant.Client
import Server (api, startApp)
import Storage (Message, initDB)
import System.Environment (getEnv)

-- postMsg :: Message -> ClientM NoContent
-- getMsgs :: ClientM [Message]
-- postMsg :<|> getMsgs = client api

main :: IO ()
main = do
    _ <- loadFile defaultConfig
    port <- getEnv "PORT"
    -- let port = "8080"
    putStrLn $ "Running in port " ++ show port
    let dbFile = "test.db"
    initDB dbFile
    -- mgr <- newManager defaultManagerSettings
    startApp (read port) dbFile

-- ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
--     _ <- postMsg "hello"
--     _ <- postMsg "world"
--     getMsgs
-- print ms
