module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "error"

digits :: Int -> [Int]
digits = reverse . go
  where
    go n
      | n < 10 = [n]
      | otherwise =
          let (d, m) = divMod n 10
              rs = go d
           in m : rs

wordNumber :: Int -> String
-- wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
wordNumber = intercalate "-" . map digitToWord . digits

-- ? wordNumber 12324546
