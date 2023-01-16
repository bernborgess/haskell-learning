module Main (main) where

import Lib

main :: IO ()
main = do
  n <- getLine
  putStrLn (n++" is "++n)
