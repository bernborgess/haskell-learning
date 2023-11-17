import Control.Monad.Identity
import Control.Monad.Trans.Maybe

-- ? Recovering the ordinary type from the transformer

-- If you have a transformer variant of a type and want to use it as if
-- it was the non-transformer version,
-- you need some 𝑚 structure that doesn’t really do anything.
-- Have we seen anything like that?
-- What about Identity?

e1 :: Identity (Maybe Integer)
e1 = runMaybeT $ (+ 1) <$> MaybeT (Identity (Just 1))

-- ==> Identity (Just 2)

e2 :: Identity (Maybe Integer)
e2 = runMaybeT $ (+ 1) <$> MaybeT (Identity Nothing)

-- ==> Identity Nothing

-- Given that, we can get Identity from IdentityT and so on
-- in the following manner:
{-
type Identity a = IdentityT Identity a
type Maybe    a = MaybeT Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State  s a = StateT s Identity a
-}

-- This works fine for recovering the non-transformer variant of each
-- type as the Identity type is acting as a bit of do-nothing structural
-- paste for filling the gap.

-- ? Yeah, but why?

-- You don't ordinarily need to do this if you're working with a transformer
-- that has a corresponding non-transformer you can use.
-- For example, it's less common to need (ExpectT Identity)
-- because the Either type is already there, so you don't need to retrieve
-- that type from the transformer.

-- However, if you're writing something with, say, Scotty, where a ReaderT
-- is part of the environment, you can't easily retrieve the Reader type
-- out of that because Reader is not a type that exists on its own and
-- you can't modify that ReaderT without essentially rewriting all of Scotty,
-- and, wow, nobody wants that for you.

-- You might then have a situation where hat you're doing only needs a Reader,
-- not a ReaderT, so you could use (ReaderT Identity) to be compatible with
-- Scotty without having to rewrite everything but still being able to keep
-- your own code a bit tighter and simpler.

-- * The transformers library

-- In general, don't use hand-rolled versions of these transformers types without
-- good reason. You can find many of them in base or the transformers library,
-- and that library should have come with your GHC installation.

-- * A note on ExceptT

-- Although a library called either exists on Hackage and provides the EitherT type,
-- most Haskellers are moving to the identical ExceptT type in the transformers library.
-- Again this has mostly to do with the fact that transformers comes packaged with GHC
-- already, so ExceptT is ready-to-hand;
-- the underlying type is the same.