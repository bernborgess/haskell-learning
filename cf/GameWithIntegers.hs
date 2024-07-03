--
--    author:  bernborgess
--    problem: GameWithIntegers - cf
--    created: 02.July.2024 21:42:40
--

import Control.Monad

main = do
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    putStrLn $ if n `mod` 3 == 0 then "Second" else "First"