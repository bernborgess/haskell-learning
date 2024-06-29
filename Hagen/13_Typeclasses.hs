-- TypeClasses

-- ? We want polymorphic functions, but with restrictions
-- (+) :: Num a => a -> a -> a

-- ? a has to have an instance of the Num typeclass


summ :: Num p => [p] -> p
summ [] = 0
summ (x:xs) = x + summ xs



-- ? :info Num
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a


-- ? :info Show
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}

-- ! We need to create either showsPrec or show, other are implied


-- ? :info Eq
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}

-- ! just need to define == or /=


-- ? :info Ord
-- class Eq a => Ord a where      
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool        
--   (<=) :: a -> a -> Bool       
--   (>) :: a -> a -> Bool        
--   (>=) :: a -> a -> Bool       
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}

-- ! It needs also the Eq typeclass.



data Temperature = C Float | F Float

instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (C c) (F f) = (1.8*c + 32) == f
  (==) (F f) (C c) = (1.8*c + 32) == f

-- ? Deriving Typeclasses
data Temperature2 =  C2 Float | F2 Float
 deriving (Show, Eq)

-- Derived equivalence:
  -- (==) (C2 n) (C2 m) = n == m
  -- (==) (F2 n) (F2 m) = n == m
  -- (==) _ _ = False
 



