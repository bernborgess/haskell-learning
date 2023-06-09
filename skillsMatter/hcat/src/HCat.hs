{-# LANGUAGE DerivingStrategies #-}

module HCat where

import Data.Char
import System.Environment
import System.IO
import System.Process

data TerminalDimension = TerminalLines | TerminalCols

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }

targetFileName :: IO FilePath
targetFileName = do
  args <- getArgs
  case args of
    [filename] -> pure filename
    _ ->
      ioError $
        userError "please provide a single filename"

getTerminalSize :: IO ScreenDimensions
getTerminalSize = do
  termLines <- tput TerminalLines
  termCols <- tput TerminalCols
  pure
    ScreenDimensions
      { screenRows = termLines,
        screenColumns = termCols
      }

tput :: TerminalDimension -> IO Int
tput dimension = do
  outputData <- readProcess "tput" [cmd] ""
  pure . read . head . lines $ outputData
  where
    cmd = case dimension of
      TerminalLines -> "lines"
      TerminalCols -> "cols"

wordWrap :: Int -> String -> [String]
wordWrap lineLength lineText =
  case splitAt lineLength lineText of
    (fullLine, "") -> [fullLine]
    (hardwrappedLine, rest) ->
      let (nextLine, remainder) = softWrap hardwrappedLine
       in nextLine : wordWrap lineLength (remainder <> rest)
  where
    softWrap hardWrapped =
      let (rest, wrappedText) = break isSpace $ reverse hardWrapped
       in (reverse wrappedText, reverse rest)

paginate :: ScreenDimensions -> String -> [String]
paginate dimensions text = pages
  where
    rows = screenRows dimensions
    cols = screenColumns dimensions
    wrappedLines = concatMap (wordWrap cols) (lines text)
    pages = map (unlines . padTo rows) $ groupsOf rows wrappedLines
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""
    groupsOf n elems
      | null elems = []
      | otherwise =
          let (hd, tl) = splitAt n elems
           in hd : groupsOf n tl

data ContinueCancel
  = Continue
  | Cancel
  deriving stock (Eq, Show)

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> return Continue
    'q' -> return Cancel
    _ -> getContinue

-- showPages :: [String] -> IO ()
-- showPages [] = pure ()
-- showPages (page : pages) = do
--   putStr "\^[[1J\^[[1;1H" -- clear the screen
--   putStr page
--   cont <-
--     if null pages
--       then pure Cancel
--       else getContinue
--   when (Continue == cont) $
--     showPages pages

-- ? After refactor
showPages :: [String] -> IO ()
showPages = forPages $ \page -> do
  putStr "\^[[1J\^[[1;1H" -- clear the screen
  putStr page

forPages :: (String -> IO ()) -> [String] -> IO ()
forPages ioAction pages =
  case pages of
    [] -> pure ()
    (page : rest) -> do
      ioAction page
      onContinue (forPages ioAction rest)

onContinue :: IO () -> IO ()
onContinue ioAction = do
  cont <- getContinue
  case cont of
    Cancel -> pure ()
    Continue -> ioAction

runHCat :: IO ()
runHCat = do
  contents <- readFile =<< targetFileName
  termSize <- getTerminalSize
  -- let wrapped = wordWrap (screenColumns termSize) contents
  -- putStrLn $ unlines wrapped
  showPages $ paginate termSize contents
