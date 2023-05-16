module Morse
  ( Morse,
    charToMorse,
    morseToChar,
    stringToMorse,
    letterToMorse,
    morseToLetter,
    -- REMOVE
    hello,
  )
where

import qualified Data.Map as M

type Morse = String

charToMorse :: M.Map Int String
charToMorse = M.fromList [(5, "a"), (3, "b"), (5, "c")]

morseToChar :: ()
morseToChar = undefined

stringToMorse :: ()
stringToMorse = undefined

letterToMorse :: ()
letterToMorse = undefined

morseToLetter :: ()
morseToLetter = undefined

hello :: IO ()
hello = putStrLn "Hello!"
