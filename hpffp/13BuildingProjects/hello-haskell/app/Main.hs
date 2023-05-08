module Main where

import Hello
import System.IO

main :: IO ()
main = do
  c <- getChar
  c' <- getChar
  if c == c'
    then do do { do { do { putStrLn "True" } } }
    else return ()

-- main :: IO ()
-- main = do
--   hSetBuffering stdout NoBuffering
--   putStr "Please input your name: "
--   name <- getLine
--   sayHello name