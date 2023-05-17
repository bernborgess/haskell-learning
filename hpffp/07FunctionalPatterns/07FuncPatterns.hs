myNum :: Integer
myNum = 1

myVal f = myNum

-- nums :: Num a => a -> Int
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- ? Let's write code
-- The following function returns the tens digit of
-- an integral argument.
tensDigit :: Integral a => a -> a
tensDigit x = d
 where
  xLast = x `div` 10
  d = xLast `mod` 10

-- a) First, rewrite it using divMod.
tensDigit' :: Integral a => a -> a
tensDigit' x = m
 where
  (d, _) = divMod x 10
  (_, m) = divMod d 10

-- b) Does the divMod version have the same type as
-- the original version?
-- c) Next, let’s change it so that we’re getting the
-- hundreds digit instead. You could start it like this
-- (though that may not be the only possibility):
-- hunsD x = d2
-- where d = undefined
