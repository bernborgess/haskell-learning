--
--    author:  bernborgess
--    problem: YogurtSale - cf
--    created: 04.July.2024 17:04:41
--

import Control.Monad

getList :: IO [Int]
getList = map read . words <$> getLine

price n a b =
  let k = min (2 * a) b
      (d, m) = divMod n 2
   in m * a + d * k

main = do
  t <- readLn
  replicateM_ t $ do
    [n, a, b] <- getList
    print $ price n a b