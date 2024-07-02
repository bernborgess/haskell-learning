--
--    author:  bernborgess
--    problem: MakeItWhite - cf
--    created: 02.July.2024 17:20:36
--

import Control.Monad
import Data.List (dropWhileEnd)

main = do
  t <- readLn
  replicateM_ t $ do
    getLine
    s <- getLine
    print $ length $ dropWhile (== 'W') $ dropWhileEnd (== 'W') s