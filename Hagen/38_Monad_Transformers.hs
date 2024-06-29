import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Functor.Classes
import Data.Functor.Identity

-- We've defined a bunch of monads:
{-
instance Monad (Either e)
instance Monad []
instance Monad Maybe
instance Monad IO
instance Monad ((->)r)
instance Monad a => Monad ((,)a)
-}

-- Then how should I define a composite like:
type IOMaybe a = IO (Maybe a)

returnIOM :: a -> IOMaybe a
returnIOM = return . Just

bindIOM :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
bindIOM iom f = do
  maybe_val <- iom
  case maybe_val of
    Nothing -> return Nothing
    (Just v) -> f v

(>>>=) = bindIOM

liftIOM :: IO a -> IOMaybe a
liftIOM io = io >>= returnIOM

checkInput :: String -> Bool
checkInput [] = False
checkInput (x : _) = isUpper x

getName :: IOMaybe String
getName = do
  input <- getLine
  if checkInput input
    then returnIOM input
    else return Nothing

example =
  putStr "Please enter you name: "
    >> getName
    >>>= ( \s ->
            liftIOM $
              putStrLn $
                "Your name is " ++ s
         )

-- We will rewrite into a more abstract code that allows
-- to give any monad the functionality of the Maybe monad.

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
  fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  mf <*> mx = MaybeT $ do
    f <- runMaybeT mf
    x <- runMaybeT mx
    return $ f <*> x

{-
  mf <*> mx = MaybeT $ do
    mb_f <- runMaybeT mf
    case mb_f of
      Nothing -> return Nothing
      Just f -> do
        mb_x <- runMaybeT mx
        case mb_x of
          Nothing -> return Nothing
          Just x -> return (Just (f x))
-}

-- not much change
instance (Monad m) => Monad (MaybeT m) where
  return = pure

  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

{-
-- Transformers
getName' :: MaybeT IO String
getName' = do
  input <- getLine
  guard (checkInput input)
  return input

example' :: MaybeT IO String
example' = runMaybeT $ do
  lift $ putStr "Please enter your name: "
  name <- getName
  lift $ putStrLn $ "Your name is " ++ name
-}

-- State Transformer
readUntil :: String -> IO ()
readUntil ending = do
  input <- getLine
  if input == ending
    then return ()
    else readUntil ending

readUntilWithCount :: String -> IO Int
readUntilWithCount ending = aux ending 0
 where
  aux ending count = do
    input <- getLine
    let ncount = count + 1
    if input == ending
      then return ncount
      else aux ending ncount

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  ~(a, _) <- runStateT m s
  return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
  ~(_, s') <- runStateT m s
  return s'

instance (Functor m) => Functor (StateT s m) where
  fmap f m =
    StateT
      ( fmap
          (\ ~(a, s') -> (f a, s'))
          . runStateT m
      )
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  StateT mf <*> StateT mx = StateT $ \s -> do
    ~(f, s') <- mf s
    ~(x, s'') <- mx s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  m >>= k = StateT $ \s -> do
    ~(a, s') <- runStateT m s
    runStateT (k a) s'

get :: m s
get = undefined

put :: s -> m ()
put = undefined

{-
readUntilWithState :: String -> IO Int
readUntilWithState ending = execStateT (aux ending) 0
 where
  -- aux :: String -> StateT Int IO String
  aux ending = do
    count <- get
    input <- liftIO getLine
    put (count + 1)
    if input == ending then return () else aux ending
-}

-- State Monad
type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState = undefined

evalState :: State s a -> s -> s
evalState = undefined

------------------  Transformers  -------------------------
-- Precursor -- Transformer -- Original T -- Combined T  --
-----------------------------------------------------------
-- Writer    -- WriterT     -- (a,w)      -- m (a,w)     --
--                                                       --
-- Reader    -- ReaderT     -- r -> a     -- r -> m a    --
--                                                       --
-- State     -- StateT      -- s -> (a,s) -- s -> m (a,s)--
--                                                       --
-- Cont      -- ContT       -- (a->r) -> r-- s -> m (a,s)--
-----------------------------------------------------------
