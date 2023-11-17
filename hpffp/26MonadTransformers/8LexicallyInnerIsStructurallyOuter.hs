import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- ? Lexically inner is structurally outer

-- One of the trickiest parts of monad transformers is that the lexical
-- representation of the types will violate your intuitions with respect
-- to the relationship it has with the structure of your values.

-- Let us note something in the definition of the following types:

-- definition in transformers may look
-- slightly different. It's not important.

{-
newtype ExceptT e m a = ExceptT
    {runExceptT :: m (Either e a)}

newtype MaybeT m a = MaybeT
    {runMaybeT :: m (Maybe a)}

newtype ReaderT r m a = ReaderT
    {runReaderT :: r -> m a}
-}

-- A necessary byproduct of how transformers work is that the additional
-- structure m is always wrapped around our value.

-- One thing to note is that it's only wrapped around things we have,
-- not things we need, such as with ReaderT.

-- The consequences of this is that a series of monad transformers in a type
-- will begin with the innermost type structurally speaking.

-- Consider the following
{-
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
-}

-- We only need to use return once
-- because it's one big Monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

-- We can sort of peel away the layers one by one:

maybeUnwrap :: (ExceptT String (ReaderT () IO)) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- Then if we'd like to evaluate this code,
-- we just feed the unit value into the function:

-- > readerUnwrap ()
-- ==> Right (Just 1)

-- ? Why is this the result? Consider that we used return for a Monad
-- comprised of Reader, Either and Maybe:

{-
instance Monad ((->) r) where
    return = const

instance Monad (Either e) where
    return = Right

instance Monad Maybe where
    return = Just
-}

-- We can treat having used return for the "one-big-Monad" of
-- Reader/Either/Maybe as composition, consider how
-- we get the same result as readerUnwrap () here:

-- > (const . Right . Just $ 1) ()
-- ==> Right (Just 1)

-- A terminological point to keep in mind when reading about monad transformers
-- is that when Haskellers say "base monad"
-- they usually what is structurally outermost.

type MyType a = IO [Maybe a]

-- In MyType, the base monad is IO.

-- ? Exercise

-- Turn readerUnwrap from the previous example back into embedded
-- through the use of the data constructors of each transformer.

-- Modify it to make it work.
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = (MaybeT . ExceptT . ReaderT . (return .)) (const (Right (Just 1)))

-- 941