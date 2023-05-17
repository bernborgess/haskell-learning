module Hangmann
  ( randomWord,
    freshPuzzle,
    runGame,
    fillInCharacter,
    handleGuess,
  )
where

import Control.Monad (forever, when)
import Data.List (intersperse, sort)
import Data.Maybe (isJust)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- STYLING THE GAME
data Color = Red | Green | Reset

instance Show Color where
  show Green = "\ESC[0;32m"
  show Red = "\ESC[0;31m"
  show Reset = "\ESC[0;0m"

data ColorString = ColorString Color String

instance Show ColorString where
  show (ColorString c s) = show c ++ s ++ show Reset

-- DEALING WITH WORDLIST
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

randomWord' :: WordList -> IO String
randomWord' (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord :: IO String
randomWord = gameWords >>= randomWord'

-- MAKING A PUZZLE
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (renderPuzzleChar <$> discovered)
      ++ " Guessed so far: "
      ++ sort guessed

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
      putStrLn $
        show (ColorString Red "! ")
          ++ "You already guessed that\
             \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn $
        show (ColorString Green "YAY ")
          ++ "This character was in the word!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn $
        show (ColorString Red "X ")
          ++ "This character wasn't in the word."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (wrongs wordToGuess guessed > length wordToGuess) $ do
    putStrLn "You Lose!"
    putStrLn $ "The word was : " ++ wordToGuess
    exitSuccess
  where
    wrongs :: String -> String -> Int
    wrongs s g = length $ filter (`notElem` s) g

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
  clearScreen
  setCursorPosition 0 0
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn
        "Your guess must\
        \ be a single character"
