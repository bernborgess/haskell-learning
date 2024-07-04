--
--    author:  bernborgess
--    problem: MaximumMultipleSum - cf
--    created: 04.July.2024 17:43:39
--

import Control.Monad

best n x =
  let s = sum $ enumFromThenTo x (2 * x) n
   in (s, x)

main = do
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    print $ snd $ maximum $ map (best n) [2 .. n]