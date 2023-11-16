{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)

-- Just as Identity helps show off the most basic essence of Functor,
-- Applicative, and Monad, IdentityT is going to help you begin to
-- understand monad transformers

-- Using this type that doesn’t have a
-- lot of interesting stuff going on with it will help keep us focused on
-- the types and the important fundamentals of transformers.

-- What we see here will be applicable to other transformers as well, but types like
-- Maybe and list introduce other possibilities (failure cases, empty lists)
-- that complicate things a bit.

-- First, let’s compare the Identity type you’ve seen up to this point and
-- our new IdentityT datatype:

-- Plain old Identity. 'a' can be something with
-- more structure, but it's not required and
-- Identity won't know anything about it.
newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Show)

-- The identity monad transformer, serving only to
-- to specify that additional structure should exist.
newtype IdentityT f a = IdentityT {runIdentityT :: f a}
    deriving (Eq, Show)

-- What changed here is that we added an extra type argument.

-- Then we want Functor instances for both Identity and IdentityT:

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

-- The IdentityT instance here should look similar to the Functor instance
-- for the One datatype above — the 𝑓 𝑎 argument is the value inside the
-- IdentityT with the (untouchable) structure wrapped around it.

-- All we know about that additional layer of structure wrapped around
-- the 𝑎 value is that it is a Functor

-- We also want Applicative instances for each:

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
    pure = IdentityT . pure
    (IdentityT mf) <*> (IdentityT mx) = IdentityT (mf <*> mx)

-- The Identity instance should be familiar. In the IdentityT instance,
-- the 𝑓 𝑎𝑏 variable represents the f (a -> b) that is the first argument
-- of (<*>). Since this can rely on the Applicative instance for 𝑚 to
-- handle that bit, this instance defines how to applicatively apply in the
-- presence of that outer IdentityT layer.

-- Finally, we want some Monad instances:

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity a) >>= fn = fn a

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    -- [1]         [2][3]    [8]      [4][5]    [7]         [6]
    (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
-}

-- The Monad instance is tricky, so we’re going to do a few things to
-- break it down. Keep in mind that Monad is where we have to really
-- use concrete type information from IdentityT in order to make the
-- types fit.

-- ? The bind breakdown
-- ? Implement the bind, step by step

-- Now we’re going to backtrack and go through implementing that bind
-- step by step

-- The goal here is to demystify what we’ve done and enable
-- you to write your own instances for whatever monad transformer you
-- might need to implement yourself.

-- We’ll go ahead and start back at the beginning, but with InstanceSigs
-- turned on so we can see the type:

-- !{-#LANGUAGE InstanceSigs #-}

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f = undefined
-}

-- Let’s leave the undefined as our final return expression, then use let
-- bindings and contradiction to see the types of our attempts at making
-- a Monad instance. We’re going to use the bottom value (undefined)
-- to defer the parts of the proof we’re obligated to produce until we’re
-- ready. First, let’s just get a let binding in place and see it load,
-- even if the code doesn’t actually work

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb = ma >>= f
         in undefined
-}

-- We’re using 𝑎𝑖𝑚𝑏 as a mnemonic for the parts of the whole thing that
-- we’re trying to implement.

-- Here we get an error:

-- ! Couldn't match type ‘m’ with ‘IdentityT m’

-- That type error isn’t the most helpful thing in the world. It’s hard to
-- know what’s wrong from that. So, we’ll poke at this a bit in order to
-- get a more helpful type error.

-- First, we’ll do something we know should work. We’ll use fmap instead.

-- Because that will typecheck (but not give us the same result as (>>=)),
-- we need to do something to give the compiler a chance to contradict
-- us and tell us the real type

-- We force that type error by asserting a fully polymorphic type for 𝑎𝑖𝑚𝑏:

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb :: a
            aimb = fmap f ma
         in undefined
-}

-- The type we just asserted for 𝑎𝑖𝑚𝑏 is impossible; we’ve just said it could
-- be every type, and it can’t. The only thing that can have that type is
-- bottom, as bottom inhabits all types.

-- Conveniently, GHC will let us know what 𝑎𝑖𝑚𝑏 actually is:

-- ! Couldn't match expected type ‘a1’
-- ! with actual type ‘m (IdentityT m b)’

-- With the current implementation, 𝑎𝑖𝑚𝑏 has the type m (IdentityT
-- m b). Now we can see the real problem: there is an IdentityT layer
-- in between the two bits of 𝑚 that we need to join in order to have a
-- monad.

-- ? Here’s a breakdown:

-- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- the pattern match on IdentityT is
-- basically our having lifted over it
-- Problem is, we >>='d

-- * (a -> IdentityT m b)

-- over

-- * m a

-- and got

-- * m (IdentityT m b)

-- It doesn’t typecheck because (>>=) merges structure of the same type
-- after lifting (remember: it’s fmap composed with join under the hood).

-- Had our type been m (m b) after binding f over ma it would’ve worked
-- fine. As it is, we need to find a way to get the two bits of 𝑚 together
-- without an intervening IdentityT layer.

-- We’re going to continue with having separate fmap and join instead of
-- using (>>=) because it makes the step-wise manipulation of structure
-- easier to see

-- How do we get rid of the IdentityT in the middle of the
-- two 𝑚 structures?

-- Well, we know 𝑚 is a Monad, which means it’s also
-- a Functor. So, we can use runIdentityT to get rid of the IdentityT
-- structure in the middle of the stack of types:

-- * Trying to change m (IdentityT m b)

-- * into m (m b)

-- Note:
-- ? runIdentityT :: IdentityT f a -> f a
-- ? fmap runIdentityT :: Functor f => f (IdentityT f1 a) -> f (f1 a)

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb :: a
            aimb = fmap runIdentityT (fmap f ma)
         in undefined
-}

-- And when we load this code, we get an encouraging type error:

-- ! Couldn't match expected type ‘a1’
-- ! with actual type ‘m (m b)’

-- It’s telling us we have achieved the type m (m b), so now we know
-- how to get where we want

-- The 𝑎1 here is the 𝑎 we had assigned to 𝑎𝑖𝑚𝑏,
-- but it’s telling us that our actual type is not what we asserted but this
-- other type.

-- Thus we have discovered what our actual type is, which
-- gives us a clue about how to fix it

-- We’ll use join from Control.Monad to merge the nested 𝑚 structure:

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb :: a
            aimb = join (fmap runIdentityT (fmap f ma))
         in undefined
-}

-- And when we load it, the compiler tells us we finally have an m b which
-- we can return:

-- ! Couldn't match expected type ‘a1’
-- ! with actual type ‘m b’

-- In fact, before we begin cleaning up our code, we can verify this is the
-- case real quick:

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb = join (fmap runIdentityT (fmap f ma))
         in aimb
-}

-- We removed the type declaration for aimb and also changed the in
-- undefined. But we know that 𝑎𝑖𝑚𝑏 has the actual type m b, so this
-- won’t work. Why? If we take a look at the type error:

-- ! Couldn't match type ‘m’ with ‘IdentityT m’

-- The (>>=) we are implementing has a final result of type IdentityT
-- m b, so the type of 𝑎𝑖𝑚𝑏 doesn’t match it yet. We need to wrap m b in
-- IdentityT to make it typecheck:

-- Remember:
-- ? IdentityT :: f a -> IdentityT f a

{-
instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let aimb = join (fmap runIdentityT (fmap f ma))
         in IdentityT aimb
-}

-- This should compile. We rewrap m b back in the IdentityT type and
-- we should be good to go.

-- ? Refactoring

-- Now that we have something that works, let’s refactor. We’d like to
-- improve our implementation of (>>=). Taking things one step at
-- a time is usually more successful than trying to rewrite all at once,
-- especially once you have a baseline version that you know should
-- work. How should we improve this line?

-- * IdentityT $ join (fmap runIdentityT (fmap f ma))

-- Well, one of the Functor laws tells us something about fmapping twice:

-- Functor law:

-- * fmap (f . g) == fmap f . fmap g

-- Indeed! So we can change that line to the following and it should be
-- identical:

-- * IdentityT $ join (fmap (runIdentityT . f) ma)

-- Now it seems suspicious that we’re fmapping and also using join on
-- the result of having fmapped the two functions we composed. Isn’t
-- join composed with fmap just (>>=)?

-- * x >>= f = join (fmap f x)

-- Accordingly, we can change our Monad instance to the following:

instance (Monad m) => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

-- And that should work still! We have a type constructor now (IdentityT)
-- that takes a monad as an argument and returns a monad as a result.

-- This implementation can be written other ways. In the transformers
-- library, for example, it’s written like this:

-- * m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m

-- Take a moment and work out for yourself how that is functionally
-- equivalent to our implementation.

-- 920
-- ? The essential extra of Monad transformers

-- It may not seem like it, but the IdentityT monad transformer actually
-- captures the essence of transformers generally

--  We only embarked on this quest because we couldn’t be guaranteed
--  a Monad instance given the composition of two types

-- Given that, we know having Functor/Applicative/Monad at our disposal isn’t
-- enough to make that new Monad instance.

-- ? So what was novel in the following code?
-- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

-- Well, it wasn’t the pattern match on IdentityT; we get that from the
-- Functor anyway:
-- Not this
-- (IdentityT ma) ...

-- It wasn’t the ability to (>>=) functions over the ma value of type 𝑚𝑎,
-- we get that from the Monad constraint on 𝑚 anyway.

-- Not this
-- ... ma >>= ...

-- We needed to know one of the types concretely so that we could use
-- runIdentityT (essentially fmapping a fold of the IdentityT structure)
-- and then repack the value in IdentityT

-- We needed to know IdentityT
-- concretely to be able to do this
-- IdentityT .. runIdentityT ...

{-
   As you’ll recall, until we used runIdentityT we couldn’t get the types
   to fit because IdentityT was wedged in the middle of two bits of 𝑚. It
   * turns out to be impossible to fix that using only Functor, Applicative,
   * and Monad
-}

-- This is an example of why we can’t just make a Monad
-- instance for the Compose type, but we can make a transformer type
-- like IdentityT where we leverage information specific to the type
-- and combine it with any other type that has a Monad instance.

-- ? In general, in order to make the types fit, we’ll need some way to fold
-- ? and reconstruct the type we have concrete information for.
