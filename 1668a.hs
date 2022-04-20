--
--    author:  bernborgess
--    problem: 1668a - forImperativeProgrammers
--    created: 19.April.2022 22:33:25
--

import Control.Monad

solve :: [Int] -> Int
solve [] = 0
solve (n : m : []) =
  if n < m then solve [m, n]
  else if n > 2 && m == 1 then -1
  else 2 * (m - 1) + 4 * ((n - m) `div` 2) + (n - m) `mod` 2

main :: IO ()
main = do ids >>= mapM_ (fn >=> print)
  where
    ids = enumFromTo 1 . read <$> getLine
    fn tc = solve . map read . words <$> getLine