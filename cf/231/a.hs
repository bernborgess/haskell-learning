--
--    author:  bernborgess
--    problem: a - cf
--    created: 20.April.2022 09:56:17
--

import Control.Monad


solve :: [Int] -> Int
solve [] = 0
solve (x:xs) = x + solve xs

-- INPUT:
-- 3          <- numero de tuplas     (n)
-- 1 0 1      <- tupla (Int,Int,Int)
-- 0 0 1
-- 1 1 1


-- O que eu queria fazer:

main :: IO ()
main = do
  sn <- getLine
  let n = [1..(read sn::Int)]



  ls <- getLine
  -- let in1 = read ls :: [Int]
  return ()




  -- let out = map (\x -> show (2*x) ) tc










  -- -- ? fn Int =>  :: [Int]
  -- let fn tc = solve . map read . words <$> getLine

  -- ans <- foldr (\x acc -> (fn x) +acc) 0 tc
  -- -- ans <- fn (head tc)
  -- -- let ans = mapM_ solve tc
  -- -- let out = sum fn tc
  -- putStrLn (show ans)
  -- return ()


  --   print sum . mapM_ fn ids
  -- where ids   = enumFromTo 1 . read <$> getLine
  --       fn tc = solve . map read . words <$> getLine


-- main :: IO ()
-- main = do ids >>= mapM_ (fn >=> print)
--   where ids   = enumFromTo 1 . read <$> getLine
--         fn tc = solve . map read . words <$> getLine
