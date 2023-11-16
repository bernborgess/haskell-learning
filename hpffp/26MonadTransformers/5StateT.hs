-- ? StateT

-- Similar to Reader and ReaderT, StateT is State but with additional
-- monadic structure wrapped around the result.
-- StateT is somewhat more useful and common than the State Monad
-- you saw earlier.
-- Like ReaderT, its value is a function:

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

-- StateT Exercises

-- If you’re familiar with the distinction, you’ll be implementing the
-- strict variant of StateT here.

-- To make the strict variant, you don’t have to do anything special.

-- Just write the most obvious thing that could work.

-- The lazy (lazier, anyway) variant is the one that involves adding
-- a bit extra.

-- We’ll explain the difference in the chapter on nonstrictness.

-- 1. You'll have to do the Functor and Applicative instances first,
-- because there aren't Functor and Applicative instances ready to go for the type
-- Monad m => s -> m (a,s).
instance (Functor m) => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $ \s -> fmap (f . fst) (sma s)
      where
        fn s = undefined
          where
            k = sma s
            x = fmap (f . fst) k