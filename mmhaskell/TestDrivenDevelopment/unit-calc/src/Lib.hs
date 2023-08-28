module Lib (
    someFunc,
    simpleMathFunction,
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

simpleMathFunction :: Int -> Int -> Int -> Int
simpleMathFunction a b c = a * b - c
