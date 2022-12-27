import Data.Bool
main = interact $ 
    bool"NO""YES" . 
    solve . map read . words

solve :: [Int] -> Bool
solve [n,m] = nxt(n+1) == m

nxt n | prime n = n
      | True    = nxt (n+1)

prime n | n<2 = False
        | 1<2 = all(\x->n`mod`x/=0)[2..floor.sqrt.fromIntegral$n]

