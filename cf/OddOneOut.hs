--
--    author:  bernborgess
--    problem: OddOneOut - cf
--    created: 02.July.2024 21:23:36
--

import Control.Monad

getList :: (Read a) => IO [a]
getList = map read . words <$> getLine

diff a b c
  | a == b = c
  | a == c = b
  | otherwise = a

main = do
  t <- readLn
  replicateM_ t $ do
    [a, b, c] <- getList :: IO [Int]
    print $ diff a b c