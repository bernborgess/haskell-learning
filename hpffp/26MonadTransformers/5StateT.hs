import Data.Bifunctor (Bifunctor (first))

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
  fmap f (StateT sma) = StateT $ \s -> fmap (first f) (sma s)

-- 2. As with Functor, you can't cheat and re-use and underlying
-- Applicative instance, so you'll have to do the work with the
-- s -> m (a,s) type yourself.
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> (StateT sma) = StateT $ \s -> do
    (fn, s') <- f s
    (a, s'') <- sma s'
    return (fn a, s'')

-- Also note that the constraint on m is not Applicative as you expect,
-- but rather Monad.

-- This is because you can't express the order dependent computation you'd
-- expect the StateT Applicative to have without having a Monad for m.

-- To learn more, see this Stack Overflow question about this issue.
-- http://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m

-- Also see this Github issue on the NICTA Course Github repository.
-- https://github.com/system-f/fp-course/issues/134

-- ! Beware! The NICTA course issue gives away the answer

-- In essence the issue is that without Monad, you're just feeding the
-- initial state to each computation in StateT rather than threading it
-- through as you go.

-- This is a general pattern constraining Applicative and Monad and
-- is worth contemplating.

-- 3. The Monad instance should look fairly similar to the Monad instance
-- you wrote for ReaderT.
instance (Monad m) => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

-- 935