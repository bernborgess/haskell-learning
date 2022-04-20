--
--    author:  bernborgess
--    problem: what_sum - THE_FIRST_PROBLEMS
--    created: 14.April.2022 22:32:20
--

import Control.Monad

solve :: [Int] -> Int
solve [] = 0
solve (x:xs) = x + solve xs

main :: IO ()
main = do 
  n <- getLine 
  fn <- solve . map read . words <$> getLine
  print fn



  --     ids >>= mapM_ (fn >=> print)
  -- where ids   = (read <$> getLine) : []
  --       fn tc = solve . map read . words <$> getLine