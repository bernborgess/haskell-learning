module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant (NoContent)
import Servant.Client
import Server (api, startApp)
import Storage (Message, initDB)

postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

main :: IO ()
main = do
    let port = 8080 :: Int
    let dbFile = "test.db"
    initDB dbFile
    mgr <- newManager defaultManagerSettings
    bracket (forkIO $ startApp port dbFile) killThread $ \_ -> do
        ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
            _ <- postMsg "hello"
            _ <- postMsg "world"
            getMsgs
        print ms

-- putStrLn $ "Running in port " ++ show port
