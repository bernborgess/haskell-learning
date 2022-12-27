import Control.Monad (forM)

readInts :: IO [Int]
readInts = fmap (map read . words) getLine

main = do
  n <- readLn :: IO Int
  a <-
    forM
      [1 .. n]
      ( \i -> do
          line <- getLine
          return (if head (tail line) == '+' then 1 else -1)
      )
  print $ sum a
