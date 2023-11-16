-- ? ReaderT

-- We mentioned back in the Reader chapter that, actually, you more
-- often see ReaderT than Reader in common Haskell use.

-- ReaderT is one of the most commonly used transformers in conventional
-- Haskell applications.

-- It is just like Reader, except in the transformer variant we’re generating
-- additional structure in the return type of the function:

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

-- The value inside the ReaderT is a function.

-- Type constructors such as Maybe are also functions in some senses,
-- but we have to handle this case a bit differently.

-- The first argument to the function inside ReaderT
-- is part of the structure we’ll have to bind over.

-- This time we’re going to give you the instances.
-- ! If you want to try writing them yourself, do not read on!

instance (Functor m) => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . pure . pure

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    -- (ReaderT f) <*> (ReaderT ma) = ReaderT $ \r -> ((<*>) <$> f) r (ma r)
    (ReaderT fmap) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmap <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r
