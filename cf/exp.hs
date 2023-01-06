import Data.Function (on)

--import Data.Composition
-- FUNCTION COMPOSITION
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
 
--on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
 
--main=interact$show.u f.p.map read.words

-- f(x/y) = e^log(x/y)
f :: Int -> Int -> Float
f=(.:)(exp.log)(on(/)fromIntegral)

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
d::Int->Int->Float
d=on(/)fromIntegral


{-
agreeLen x y = length $ takeWhile (\(a,b) -> a == b) (zip x y)
agreeLen x y = length . takeWhile (\(a,b) -> a == b) $ zip x y
agreeLen x y = length . takeWhile (uncurry (==)) . zip x $ y
agreeLen x = length . takeWhile (uncurry (==)) . zip x 
agreeLen x = (.) (length . takeWhile (uncurry (==))) (zip x)

SINCE f (g x) == f . g $ x
WITH  f = (.) (length . takeWhile (uncurry (==))
AND   g = zip

agreeLen x = ((.) (length . takeWhile (uncurry (==)))) . zip $ x
agreeLen = ((.) (length . takeWhile (uncurry (==)))) . zip 
agreeLen = ((length . takeWhile (uncurry (==))).) . zip 
-}


