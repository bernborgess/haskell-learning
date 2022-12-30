import Data.Function (on)
--main=interact$show.u f.p.map read.words

p :: [a] -> (a,a)
p [a,b] = (a,b)

u :: (a->b->c)->(a,b)->c 
u f(a,b)=f a b

{-
f :: Int->Int->Float
f=exp.log.div
f a b=(fromIntegral(div a b)

div :: Int -> Int -> Int
fromIntegral :: Int -> Float
(.) :: (b->c) -> (a->b) -> a -> c
(on) :: (b -> b -> c) -> (a -> b) -> a -> a -> c

f :: (Integral a, Num b) => (a->b)->(a->a->a)->b
f :: (Int->Int->Int)->(Int->Float)->Int->Int->Float
-}
f::Int->Int->Float
f=on(/)fromIntegral

