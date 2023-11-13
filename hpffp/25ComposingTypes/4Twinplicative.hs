-- 25.4 Twinplicative
-- You probably guessed this was our next step in Compose-landia.
-- Applicatives, it turns out, are also closed under composition. We can
-- indeed compose two types that have Applicative instances and get a
-- new Applicative instance. But you’re going to write it.
-- ! GOTCHA! Exercise time
-- instance types provided as they may help.
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Applicative (liftA2))

-- Correspond to (.)
newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

-- We mentioned in an earlier chapter that Applicative is a weaker algebra
-- than Monad, and that sometimes there are benefits to preferring an
-- Applicative when you don’t need the full power of the Monad. This is
-- one of those benefits. To compose Applicatives, you don’t need to do
-- the legwork that monads require in order to compose and still have
-- a Monad. Oh, yes, right — we still haven’t quite made it to monads
-- composing, but we’re about to.