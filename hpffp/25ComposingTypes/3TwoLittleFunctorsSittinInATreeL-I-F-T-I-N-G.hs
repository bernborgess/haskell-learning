-- Correspond to id
newtype Identity a = Identity {runIdentity :: a}

-- Correspond to (.)
newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

-- Let's start with composing functors, using the types we've seen above
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

ex1 :: Compose [] Maybe Int
ex1 = Compose [Just 1, Nothing]

-- Compose {getCompose = [Just 2,Nothing]}
val1 = fmap (+ 1) ex1

-- We can generalize this to different amounts of structure
newtype One f a = One (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

-- One or more layer of structure than compose
newtype Three f g h a = Three (f (g (h a)))
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- Composition of two datatypes that have a Functor instance
-- gives rise to a new Functor instance
-- i.e. Functors are closed under composition