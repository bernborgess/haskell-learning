module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList
  = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
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

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess,
         alreadyGuessed puzzle guess
       ) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the word,\
        \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in the word,\
        \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length guessed > 7) $ do
    putStrLn "You Lose!"
    putStrLn $ "The word was : " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn "You win!"
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn
        "Your guess must\
        \ b a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (toLower <$> word)
  runGame puzzle

-- TODO
-- You may have noticed when you were playing with the hangman
-- game, that there are some weird things about its game logic:

-- [ ] although it can play with words up to 9 characters long, you only
-- get to guess 7 characters;
-- [ ] it ends the game after 7 guesses, whether they were correct or
-- incorrect;
-- [ ] if your 7th guess supplies the last letter in the word, it may still
-- tell you you lost;
-- [ ] it picks some very strange words that you didn’t suspect were
-- even in the dictionary.

-- These make it unlike hangman as you might have played it in the past.
-- Ordinarily, only incorrect guesses count against you, so you can make
-- as many correct guesses as you need to fill in the word. Modifying
-- the game so that it either gives you more guesses before the game
-- ends or only uses shorter words (or both) involves only a couple of
-- uncomplicated steps.
-- A bit more complicated but worth attempting as an exercise is changing
-- the game so that, as with normal hangman, only incorrect guesses
-- count towards the guess limit.