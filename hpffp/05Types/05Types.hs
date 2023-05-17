{- Experiment
data MyList a = Empty | Head a (MyList a)

instance Foldable MyList where
  foldr :: (a -> b -> b) -> b -> MyList a -> b
  foldr _ z Empty = z
  foldr f z (Head x xs) = x `f` foldr f z xs

instance Num (MyList a) where
  x + y =
    case x of
      Empty -> Empty
      (Head x' xs) ->
        case y of
          Empty -> Empty
          (Head y' ys) -> Head x' (Head y' xs)

  x * y = Empty
  abs x = Empty
  signum x = Empty
  fromInteger i = Empty
  negate x = Empty
-}

-- ? Exercise 1
-- Functions
-- not  length  concat  head  (<)
-- Type Signatures
-- ([a]->a) ([[a]]->a)  (Bool->Bool) ([a]->Int)
-- (Ord a => a -> a -> Bool)

-- not :: Bool -> Bool
-- length :: [a] -> Int
-- concat :: [[a]] -> [a]
-- head :: [a] -> a
-- (<) :: Ord a => a -> a -> Bool

-- ? Currying
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested =
  \i -> \b -> i + nonsense b

-- Intermission Exercises
