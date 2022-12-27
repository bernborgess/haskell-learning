
main = do
  n <- getLine
  interact $ unlines . map (
    (\b->last$"NO":["YES"|b]).solve. map (read).words
    ) . lines

solve :: [Int] -> Bool
solve (a:b:c:d:_) = fM >= sm && sM >= fm
        where (fm,fM) = orde (a,b)
              (sm,sM) = orde (c,d)

orde :: (Int,Int) -> (Int,Int)
orde (x,y) = if x <= y then (x,y) else (y,x)