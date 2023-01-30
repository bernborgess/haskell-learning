
infix 9 =^.^=
(=^.^=) :: Int -> Int -> Int
(=^.^=) x y = (x + y) `mod` 2
{-# INLINE (=^.^=) #-}

m = 3 =^.^= 2
