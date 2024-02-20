--
--    author:  bernborgess
--    problem: a - haskellLearning
--    created: 20.February.2024 13:41:21
--

import Control.Monad
import Data.List
import Data.Ord (Down (Down))

tok x = (length x, head x)

main = do
    t <- readLn
    replicateM_ t $ do
        getLine >>= putStrLn . pure . snd . head . sortOn Down . map tok . group . sort