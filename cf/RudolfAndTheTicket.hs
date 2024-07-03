--
--    author:  bernborgess
--    problem: RudolfAndTheTicket - cf
--    created: 03.July.2024 13:18:52
--

import Control.Monad

getList :: IO [Int]
getList = map read . words <$> getLine

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [n, m, k] <- getList
    bs <- filter (<= k) <$> getList
    cs <- filter (<= k) <$> getList
    print $ length $ do
      b <- bs
      c <- cs
      ([() | b + c <= k])