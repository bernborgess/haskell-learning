import Control.Monad 

getInt :: IO Int
getInt = fmap read getLine

getIntLine :: Int -> IO Int
getIntLine _ = do
  arr <- getLine
  return (sum.fmap read $ words arr)

main :: IO ()
main = do
  n <- getInt
  ans <- forM [1..n] getIntLine
  print (length $ filter (>1) ans)
  return ()
 