newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Compose Foldable

-- Write the Foldable instance for Compose. The foldMap = undefined
-- bit is a hint to make it easier and look more like what you’ve seen
-- already.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = foldMap (foldMap f) fga

-- Compose Traversable

-- Write the Traversable instance for Compose.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: (Applicative h) => (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

-- And now for something completely different
-- This has nothing to do with anything else in this chapter.

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c
    second = bimap id