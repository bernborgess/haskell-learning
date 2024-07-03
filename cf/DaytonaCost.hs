--
--    author:  bernborgess
--    problem: DaytonaCost - cf
--    created: 02.July.2024 21:36:32
--

import Control.Monad
import Data.Bool (bool)

getList :: IO [Int]
getList = map read . words <$> getLine

main = do
  t <- readLn
  replicateM_ t $ do
    [n, k] <- getList
    xs <- getList
    putStrLn $ bool "NO" "YES" $ k `elem` xs