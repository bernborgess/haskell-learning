import Data.Char

-- ? IO Monad

hw :: IO ()
hw = putStrLn "Hello World!"

-- getLine :: IO String

-- * Defining IO Action

-- * do : strict evaluation

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  let uname = map toUpper name
  putStrLn ("Hello " ++ uname ++ ".")


-- ! Should never use it!
-- unsafePerformIO :: IO a -> a


-- ? main IO action

main :: IO ()
main = do
  i <- getLine
  if i /= "quit" then do
    putStrLn ("Input: " ++ i)
    main
  else
    return ()





count :: Int -> Int -> IO ()
count n m = do
  putStrLn (show n)
  if n < m then
    count (n+1) m
  else
    return ()

