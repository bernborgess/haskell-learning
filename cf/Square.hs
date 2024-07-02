--
--    author:  bernborgess
--    problem: Square - cf
--    created: 02.July.2024 17:35:43
--

import Control.Monad

getList :: (Read a) => IO [a]
getList = map read . words <$> getLine

main = do
  t <- readLn
  replicateM_ t $ do
    xys <- replicateM 4 getList
    let [maxX, maxY] = maximum xys
    let [minX, minY] = minimum xys
    print $ (maxX - minX) * (maxY - minY)