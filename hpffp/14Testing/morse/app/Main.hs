module Main where

import qualified Morse (hello)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Morse.hello
