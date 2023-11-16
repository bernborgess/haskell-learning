-- ? EitherT

-- Just as Maybe has a transformer variant in the form of MaybeT,
-- we can make a transformer variant of Either.
-- We’ll call it EitherT.
-- Your task is to implement the instances for the transformer variant:

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

-- EitherT Exercises
-- 1. Write the Functor instance for EitherT:
instance (Functor m) => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2. Write the Applicative instance for EitherT:
instance (Applicative m) => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure = EitherT . pure . pure

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT fab) <*> (EitherT ma) = EitherT $ ((<*>) <$> fab) <*> ma

-- 3. Write the Monad instance for EitherT:
instance (Monad m) => Monad (EitherT e m) where
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ma) >>= f = EitherT $ do
        v <- ma
        case v of
            Left e -> return $ Left e
            Right a -> runEitherT $ f a

-- 4. Write the swapEitherT helper function for EitherT.
-- transformer version of swapEither
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where
    swapEither :: Either e a -> Either a e
    swapEither (Left e) = Right e
    swapEither (Right a) = Left a

-- 5. Write the transformer variant of the either catamorphism
eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = do
    v <- amb
    case v of
        Left a -> fa a
        Right b -> fb b
