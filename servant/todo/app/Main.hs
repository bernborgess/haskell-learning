module Main (main) where

import Api.Storage (initDB)
import Configuration.Dotenv (defaultConfig, loadFile)
import Server (startApp)
import System.Environment (getEnv)

main :: IO ()
main = do
    -- .env setup
    _ <- loadFile defaultConfig

    port <- getEnv "PORT"

    dbFile <- getEnv "DATABASE_FILE"
    initDB dbFile

    putStrLn $ "Running in port " ++ show port
    startApp
