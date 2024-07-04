--
--    author:  bernborgess
--    problem: CanISquare - cf
--    created: 04.July.2024 17:19:46
--

import Control.Monad
import Data.Bool
import GHC.Float

main = readLn >>= flip replicateM_ (getLine >> getLine >>= putStrLn . bool "NO" "YES" . ap (==) ((^ 2) . floor . sqrt . int2Double) . sum . map read . words)
