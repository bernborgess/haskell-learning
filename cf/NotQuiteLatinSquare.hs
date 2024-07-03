--
--    author:  bernborgess
--    problem: NotQuiteLatinSquare - cf
--    created: 03.July.2024 13:45:43
--

import Control.Monad
import Data.List (find)
import Data.Maybe (fromMaybe)

solve = do
  ls <- replicateM 3 getLine
  let ans = do
        s <- find (elem '?') ls
        find (`notElem` s) "ABC"
  return [fromMaybe ' ' ans]

main = readLn >>= flip replicateM_ (solve >>= putStrLn)