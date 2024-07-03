--
--    author:  bernborgess
--    problem: StairPeakNeither - cf
--    created: 02.July.2024 21:31:06
--

import Control.Monad

getList :: (Read a) => IO [a]
getList = map read . words <$> getLine

decide a b c
  | a < b && b < c = "STAIR"
  | a < b && b > c = "PEAK"
  | otherwise = "NONE"

main = do
  t <- readLn
  replicateM_ t $ do
    [a, b, c] <- getList :: IO [Int]
    putStrLn $ decide a b c