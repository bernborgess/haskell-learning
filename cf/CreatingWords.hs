--
--    author:  bernborgess
--    problem: CreatingWords - cf
--    created: 02.July.2024 17:30:17
--

import Control.Monad

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [a : as, b : bs] <- words <$> getLine
    putStrLn $ (b : as) ++ (' ' : a : bs)