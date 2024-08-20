--
--    author:  bernborgess
--    problem: a+b_again? - haskellLearning
--    created: 19.August.2024 22:19:39
--

import Control.Monad

-- main = readLn >>= flip replicateM_ (getLine >>= print . foldl ((. (read . pure)) . (+)) 0)
main = interact $ unlines . map (show . foldl (\a c -> a + read [c]) 0) . tail . lines
