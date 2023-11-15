import Control.Monad.Reader

-- Monad transformers

-- We’ve now seen what the problem with Monad is: you can put two
-- together but you can’t get a new Monad instance out of it. When we
-- need to get a new Monad instance, we need a monad transformer. It’s
-- not magic; the answer is in the types.

{-
 * We said above that a monad transformer is a type constructor that
 * takes a Monad as an argument and returns a Monad as a result
-}

-- We also noted that the fundamental problem with composing two Monads
-- lies in the impossibility of joining two unknown Monads. In order
-- to make that join happen, we need to reduce the polymorphism
-- and get concrete information about one of the Monads that we’re
-- working with. The other Monad remains polymorphic as a variable
-- type argument to our type constructor

{-
 * Transformers help you make
 * a monad out of multiple (2, 3, 4...) types that each have a Monad
 * instance by wrapping around existing monads that provide each bit
 * of wanted functionality.
-}

-- The types are tricky here, so we’re going to be walking through writing
-- monad transformers very slowly. Parts of what follows may seem
-- tedious, so work through it as slowly or quickly as you need to.

-- ? Monadic stacking

-- Applicative allows us to apply functions of more than one argument
-- in the presence of functorial structure, enabling us to cope with this
-- transition:

-- from this:
e1 :: Maybe Integer
e1 = fmap (+ 1) (Just 1)

-- to this:
e2 :: Maybe (Integer, String, [Integer])
e2 = (,,) <$> Just 1 <*> Just "lol" <*> Just [1, 2]

-- Sometimes we want a (>>=) which can address more than one Monad
-- at once. You’ll often see this in applications that have multiple things
-- going on, such as a web app where combining Reader and IO is common.
-- You want IO so you can perform effectful actions like talking to a
-- database and also Reader for the database connection(s) and/or HTTP
-- request context. Sometimes you may even want multiple Readers
-- (app-specific data vs. what the framework provides by default), although
-- usually there’s a way to just add the data you want to a product
-- type of a single Reader.

-- ? So the question becomes, how do we get one big bind over a type like the following?

type Q1 a = IO (Reader String [a])

-- where the Monad instances involved
-- are that of IO, Reader, and []

-- ! Doing in badly

-- We could make one-off types for each combination, but this will get
-- tiresome quickly. For example:

newtype MaybeIO a = MaybeIO
    {runMaybeIO :: IO (Maybe a)}

newtype MaybeList a = MaybeList
    {runMaybeList :: [Maybe a]}

-- We can get a Monad for two types, as
-- long as we know what one of the types is
