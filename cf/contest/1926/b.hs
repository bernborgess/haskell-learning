--
--    author:  bernborgess
--    problem: b - haskellLearning
--    created: 20.February.2024 13:55:23
--

import Control.Monad
import Data.Bool

isSqr (x : xs) = all (== x) xs
solve = putStrLn . bool "TRIANGLE" "SQUARE" . isSqr . filter (> 0) . map (length . filter (== '1'))
main = do
    t <- readLn
    replicateM_ t $ do
        n <- readLn
        xs <- replicateM n getLine
        solve xs