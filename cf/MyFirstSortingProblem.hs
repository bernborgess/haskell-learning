--
--    author:  bernborgess
--    problem: MyFirstSortingProblem - cf
--    created: 04.July.2024 17:10:04
--

import Control.Monad
import Data.List

getList :: IO [Int]
getList = map read . words <$> getLine

printList = putStrLn . unwords . map show

main = readLn >>= flip replicateM_ (getList >>= printList . sort)