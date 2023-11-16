-- Transformers are bearers of single-type concrete information that let
-- you create ever-bigger Monads in a sense.

-- Nesting such as
-- (Monad m) = m (m a)
-- is addressed by join already

-- We use transformers when we want a >>= operation over 𝑓 and 𝑔 of
-- different types (but both have Monad instances).

-- You have to create new types called monad transformers
-- and write Monad instances for those types to have a way of dealing
-- with the extra structure generated.

-- ? The general pattern is this:
-- You want to compose two polymorphic types, 𝑓 and 𝑔, that each have a
-- Monad instance. But you’ll end up with this pattern:

-- * f (g (f b))

-- ! Monad’s bind can’t join those types, not with that intervening 𝑔
-- So you need to get to this:

-- * f (f b)

-- You won’t be able to unless you have some way of folding the 𝑔 in the middle.
-- You can’t do that with just Monad.

-- The essence of Monad is join, but here you have only one bit of 𝑔 structure,
-- not g (g ...), so that’s not enough.

-- The straightforward thing to do is to make 𝑔 concrete.

-- With concrete type information for the “inner” bit of
-- structure, we can fold out the 𝑔 and get on with it

-- The good news is that transformers don’t require 𝑓 be concrete;
-- 𝑓 can remain polymorphic so long as it has a Monad instance,
-- so we only write a transformer once for each type.

-- We can see this pattern with IdentityT as well.
-- You may recall this step in our process of writing IdentityT’s Monad:
-- (IdentityT ma) >>= f =
--     let aimb :: m (IdentityT m b)
--         aimb = fmap f ma

-- We have something that’ll typecheck, but it’s not quite in the shape we
-- would like.

-- Of course, the underlying type once we throw away
-- the IdentityT data constructor
-- is m (m b) which’ll suit us just fine,
-- but we have to fold out the IdentityT
-- before we can use the join from Monad m => m

-- That leads us to the next step:
-- let aimb :: m (m b)
--     aimb = fmap runIdentityT (fmap f ma)

{-
 * Now we finally have something we can join because we lifted the
 * record accessor for IdentityT over the 𝑚!
-}

-- Since IdentityT is so simple, the record accessor is sufficient
-- to “fold away” the structure
-- From there the following transitions become easy:

-- * m (m b) -> m b -> IdentityT m b

-- The final type is what our definition of (>>=) for IdentityT must result in.

-- The basic pattern that many monad transformers are enabling us to
-- cope with is the following type transitions, where 𝑚 is the polymorphic,
-- “outer” structure and 𝑇 is some concrete type the transformer is for.

-- For example, in the above, 𝑇 would be IdentityT.

{-
    m (T m b)
->  m (m b)
->  m b
->  T m b
-}

-- Don’t consider this a hard and fast rule for what types you’ll encounter
-- in implementing transformers, but rather some intuition for why
-- transformers are necessary to begin with.
