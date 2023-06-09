module Main where

main :: IO ()
main = readFile "example.txt" >>= putStrLn
