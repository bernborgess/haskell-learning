--
--    author:  bernborgess
--    problem: array_sum - forImperativeProgrammers
--    created: 14.April.2022 11:04:10
--

import Control.Monad

solve :: [Int] -> Int
solve [] = 0
solve (x:xs) = x + solve xs

main :: IO ()
main = do ids >>= mapM_ (fn >=> print)
  where ids   = enumFromTo 1 . read <$> getLine
        fn tc = solve . map read . words <$> getLine