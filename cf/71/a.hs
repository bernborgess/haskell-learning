--
--    author:  bernborgess
--    problem: a - 71
--    created: 20.April.2022 08:30:35
--

import Control.Monad

solve :: [Char] -> [Char]
solve [] = ""
solve (xs) = if len > 10 then head xs : show (len - 2) ++ [last xs]
  else xs
  where len = length xs


main :: IO ()
main = do ids >>= mapM_ (fn >=> putStrLn)
  where ids   = enumFromTo 1 . read <$> getLine
        fn tc = solve <$> getLine

