-- Identity Functor
newtype Id a = Id a

-- Endofunctor
newtype T a = T a

η :: Id a -> T a
η (Id a) = T a

μ :: T (T a) -> T a
μ (T (T a)) = T a

-- Functor Composition
infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
{-# INLINE (.:) #-}

f a = μ (T (T a))
