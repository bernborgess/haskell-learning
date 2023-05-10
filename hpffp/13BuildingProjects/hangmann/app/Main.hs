module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)

-- import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
-- gameWords = do
--   aw <- allWords
--   return (filter gameLength aw)
gameWords = do filter gameLength <$> allWords
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  -- TODO
  -- randomIndex <- randomRIO (0,(length wl)-1)
  let randomIndex = 0
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- MAKING A PUZZLE
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (renderPuzzleChar <$> discovered)
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (Nothing <$ s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs) c = c `elem` gs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word nf (c : s)
  where
    zp wc mc = if wc == c then Just wc else mc
    nf = zipWith zp word filledInSoFar

myBlah :: String
myBlah = intersperse ' ' "Blah"

main :: IO ()
main = putStrLn myBlah
