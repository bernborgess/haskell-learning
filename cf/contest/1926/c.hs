--
--    author:  bernborgess
--    problem: c - haskellLearning
--    created: 20.February.2024 14:16:06
--

import Control.Monad
import Data.Char (digitToInt)

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

main = do
    t <- readLn
    replicateM_ t $
        readLn >>= print . sum . map sumDigits . enumFromTo 1