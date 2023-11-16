import Control.Monad.Identity

-- ? MaybeT

-- In the last chapter, we worked through an extended breakdown
-- of the IdentityT transformer.

-- IdentityT is, as you might imagine, not the most useful of the
-- monad transformers, although it is not without
-- practical applications (more on this later).

-- As we’ve seen at various times, though, the Maybe Monad can be
-- extremely useful and so it is that the transformer variant,
-- MaybeT, finds its way into the pantheon of important transformers

-- The MaybeT transformer is a bit more complex than IdentityT

-- If you worked through all the exercises of the previous chapter,
-- then this section will not be too surprising, because this will
-- rely on things you’ve seen with IdentityT and the Compose type already

-- However, to ensure that transformers are thoroughly demystified
-- for you, it’s worth working through them carefully

-- We begin with the newtype for our transformer:

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- The structure of our MaybeT type and the Compose type are similar
-- so we can reuse the basic patterns of the Compose type for the
-- Functor and Applicative instances:

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- We don’t need to do anything different for the Functor instance,
-- because transformers are needed for the Monad, not the Functor

-- We’ll start with what might seem like an obvious way to write the
-- MaybeT Applicative and find out why it doesn’t work.
-- This does not compile:

{-
instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fab <*> mma
-}

-- The 𝑓 𝑎𝑏 represents the function m (Maybe (a -> b)) and the 𝑚𝑚𝑎
-- represents the m (Maybe a).
-- You’ll get this error if you try it:

-- ! Couldn't match type: Maybe (a -> b)
-- ! with: Maybe a -> Maybe b

-- The idea here is that we have to lift an Applicative “apply”
-- over the outer structure 𝑓 to get the g (a -> b)
-- into g a -> g b so that the
-- Applicative instance for 𝑓 can be leveraged.
-- We can stretch this idea a bit and use concrete types:

innerMost ::
    [Maybe (Identity (a -> b))] ->
    [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' ::
    [Maybe (Identity a -> Identity b)] ->
    [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' ::
    [Maybe (Identity a) -> Maybe (Identity b)] ->
    [Maybe (Identity a)] ->
    [Maybe (Identity b)]
final' = (<*>)

-- The function that could be the actual Applicative instance
-- for such a hypothetical type would look like:

lmiApply ::
    [Maybe (Identity (a -> b))] ->
    [Maybe (Identity a)] ->
    [Maybe (Identity b)]
lmiApply f = final' (second' (innerMost f))

-- The Applicative instance for our MaybeT type will employ this
-- same idea, because Applicatives are closed under composition,
-- as we noted in the last chapter.

-- We only need to do something different from the
-- Compose instances once we get to Monad.

-- So, we took the long way around to this:

instance (Applicative m) => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

-- ? MaybeT Monad instance

-- At last, on to the Monad instance!
-- Note that we’ve given some of the intermediate types:

instance (Monad m) => Monad (MaybeT m) where
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y -> runMaybeT (f y)
