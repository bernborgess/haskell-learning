--
--    author:  bernborgess
--    problem: bookWordCount - sandbox
--    created: 13.September.2023 15:59:26
--

import Control.Monad
import Data.Char (isAlpha, isLetter)
import Data.List (nub)

-- 1. Read Book file
-- 2. Split into words
-- 3. Count occurrences of words
-- 4. Save the result to file

valid ch = ch == ' ' || isAlpha ch

main = do
    putStrLn "Enter path to book file (.txt)"
    bookFilePath <- getLine
    file <- readFile bookFilePath
    print $ nub $ words $ filter valid file
