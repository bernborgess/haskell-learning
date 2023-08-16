module Main (main) where

import Lib

main :: IO ()
main = do
    let port = 8080 :: Int
    putStrLn $ "Running in port " ++ show port
    startApp 8080
