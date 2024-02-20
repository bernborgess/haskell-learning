oMap (f :: a -> b) (xs :: [Maybe a]) = fmap k xs
  where
    k = fmap f
