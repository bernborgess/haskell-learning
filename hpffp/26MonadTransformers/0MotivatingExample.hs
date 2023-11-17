-- https://mmhaskell.com/monads/transformers
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char

-- Consider this:
main1 :: IO ()
main1 = do
    maybeUserName <- readUserName
    case maybeUserName of
        Nothing -> print "Invalid user name!"
        Just uName -> do
            maybeEmail <- readEmail
            case maybeEmail of
                Nothing -> print "Invalid email!"
                Just email -> do
                    maybePassword <- readPassword
                    case maybePassword of
                        Nothing -> print "Invalid password!"
                        Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
    putStrLn "Please enter you username:"
    str <- getLine
    if length str > 5
        then return $ Just str
        else return Nothing

readEmail :: IO (Maybe String)
readEmail = do
    putStrLn "Please enter you email:"
    str <- getLine
    if '@' `elem` str && '.' `elem` str
        then return $ Just str
        else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
    putStrLn "Please enter you password:"
    str <- getLine
    if length str < 8 || not (any isUpper str) || not (any isLower str)
        then return Nothing
        else return $ Just str

login :: String -> String -> String -> IO ()
login n e p = putStrLn "Logged in successfully"

-- In this example, all our potentially problematic code takes place
-- within the IO monad. How can we use the Maybe monad when we're
-- already in another monad?

-- ? MONAD TRANSFORMERS

-- Luckily, we can get the desired behavior by using monad transformers
-- to combine monads. In this example, we'll wrap the IO actions within a
-- transformer called MaybeT.

-- A monad transformer is fundamentally a wrapper type.
-- It is generally parameterized by another monadic type.

-- You can then run actions form the inner Monad, while adding your own
-- customized behavior for combining actions in this new monad.

-- The common transformers add T to the end of an existing monad.

-- Here's the definition of MaybeT:

{-
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y -> runMaybeT (f y)
-}

-- So MaybeT itself is simply a newtype. It in turn contains a wrapper around
-- a Maybe value. If the type m is a monad,
-- we can also make a monad out of MaybeT.

-- Let's consider our example, We want to use MaybeT to wrap the IO monad,
-- so we can run IO actions.

-- This means our new monad is MaybeT IO. Our three helper functions
-- all return strings, so they each get the type MaybeT IO String.

-- To convert the old IO code into the MaybeT monad, all we need to do is
-- wrap the IO action in the MaybeT constructor.

readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
    putStrLn "Please enter your username:"
    str <- getLine
    if length str > 5
        then return $ Just str
        else return Nothing

-- ...

-- Now we can wrap all three of these call into a single monadic action
-- and do a single patter match to get the results.

-- We'll use the runMaybeT function to unwrap the Maybe value from the MaybeT:

main2 :: IO ()
main2 = do
    maybeAuth <- runMaybeT $ do
        user <- MaybeT readUserName
        email <- MaybeT readEmail
        pass <- MaybeT readPassword
        return (user, email, pass)

    case maybeAuth of
        Nothing -> print "Couldn't login"
        Just (n, e, p) -> login n e p

-- And this new code will have the proper short-circuiting behavior of the
-- Maybe monad! If any of the read functions fail, our code will immediately
-- return Nothing

-- ? ADDING MORE LAYERS

-- Here's the best part about monad transformers.

-- Since our newly created type is a monad itself, we can wrap it
-- inside another transformer!

-- Pretty much all common monads have transformer types in the same way the
-- MaybeT is a transformer for the ordinary Maybe monad.

-- For a quick example, suppose we had an Env type containing some user info.
-- We could wrap this environment in a Reader. However, we want to still have
-- Access to IO functionality, so we'll use the ReaderT transformer.

-- Then we can wrap the result in MaybeT transformer

type Env = (Maybe String, Maybe String, Maybe String)

readUserName'' :: MaybeT (ReaderT Env IO) String
readUserName'' = MaybeT $ do
    (maybeOldUser, _, _) <- ask
    case maybeOldUser of
        Just str -> return $ Just str
        Nothing -> do
            -- lift allows normal IO function from inside
            -- ReaderT Env IO
            lift $ putStrLn "Please enter you username:"
            input <- lift getLine
            if length input > 5
                then return (Just input)
                else return Nothing

-- Notice we had to use lift to run the IO function getLine.
-- In a monad transformer, the lift function allows you to run actions
-- in the underlying monad.

-- This behavior is encompassed by the MonadTrans class:
{-
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a
-}
-- So using lift in the ReaderT Env IO action allows IO functions.
-- Using the type template from the class, we can substitute Reader Env
-- for t, and IO for m.

-- Within a MaybeT (ReaderT Env IO) function, calling lift would allow you
-- to run a Reader function.
-- We don't need this above since the bulk of the code lies in Reader actions
-- wrapped by the MaybeT constructor

-- To understand the concept of lifting, think of your monad layer as a stack
-- When you have a ReaderT Env IO action, imagine a Reader Env monad on top of
-- the IO monad.

-- An IO action exists on the bottom layer, you can lift multiple time.
-- Calling lift twice from the MaybeT (ReaderT Env IO) monad will allow
-- you to call IO functions.

-- It's inconvenient to have to know how many time to call lift to get to
-- a particular level of the chain.

-- Thus helper functions are frequently used for this.
-- Additionally, since monad transformers can run several layers deep,
-- the types can get complicated.

-- So it is typical to use type synonyms liberally.

type TripleMonad a = MaybeT (ReaderT Env IO) a

performReader :: ReaderT Env IO a -> TripleMonad a
performReader = lift

performIO :: IO a -> TripleMonad a
performIO = lift . lift

-- ? TYPECLASSES

-- As a similar idea, there are some typeclasses which allow you to make
-- certain assumptions about the monad stack below.

-- For instance, you often don't care what the exact stack is, but you just
-- need IO to exist somewhere on the stack

-- This is the purpose of the MonadIO typeclass:
{-
class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a
-}

-- We can use this behavior to get a function to print even when we don't
-- know its exact monad:
debugFunc :: (MonadIO m) => String -> m ()
debugFunc input = liftIO $ putStrLn ("Successfully produced input: " ++ input)

-- So even though this function doesn't live in MaybeT IO, we can write a
-- version of our main function to use it:

main3 :: IO ()
main3 = do
    maybeAuth <- runMaybeT $ do
        user <- MaybeT readUserName
        debugFunc user
        email <- MaybeT readEmail
        debugFunc email
        pass <- MaybeT readPassword
        debugFunc pass
        return (user, email, pass)

    case maybeAuth of
        Nothing -> putStrLn "Could not login!"
        Just (n, e, p) -> login n e p

-- One final note:
-- ! You cannot, in general, wrap another monad with the IO monad
-- ! using a transformer.
-- ? You can, however, make the other monadic value
-- ? the return type of an IO action

-- func :: IO (Maybe String)
-- This type makes sense

-- func2 :: IO_T (ReaderT Env (Maybe)) String
-- This does not exist

-- ? SUMMARY

-- Now that you know how to combine your monads together, you're almost done
-- with understanding the key concepts of monads!

-- You could probably go out now and start writing some pretty complex code!

-- But to truly master monads, you should know how to make your own, and
-- there's one final concept that you should understand for that.

-- This is the idea of type "laws"
