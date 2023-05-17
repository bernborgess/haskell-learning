module Main where

import Data.Char
import Hangmann (freshPuzzle, randomWord, runGame)

main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (toLower <$> word)
  runGame puzzle