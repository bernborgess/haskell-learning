-- Higher kinded type
-- ? f has the kind * -> *
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- ? There's a whole lot of fmap goind around
fme1 = fmap (> 3) [1 .. 6]

fme2 = fmap (+ 1) (Just 1)

fme3 = fmap (10 /) (4, 5)

fme4 = fmap (++ ", Esq.") (Right "Chris")

-- 16.6 The Good, the Bad and the Ugly
data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter (f a)
  fmap _ WhatThisIsCalled = WhatThisIsCalled
