--
--    author:  bernborgess
--    problem: rock - haskellLearning
--    created: 09.August.2024 17:24:40
--    https://dmoj.ca/problem/hkccc15j1
--

import Control.Monad
import Text.Printf (printf)

getList = words <$> getLine

wins "rock" "scissors" = True
wins "scissors" "paper" = True
wins "paper" "rock" = True
wins _ _ = False

main :: IO ()
main = do
  getLine
  as <- getList
  bs <- getList
  let fn (a, b) (r, s) = (a + da, b + db)
        where
          da = fromEnum $ wins r s
          db = fromEnum $ wins s r
  let (a, b) = foldl fn (0, 0) (zip as bs)
  printf "%d %d\n" a b