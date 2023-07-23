-- In this chapter, we will:
-- • explain the Traversable typeclass and its canonical functions;
-- • explore examples of Traversable in practical use;
-- • tidy up some code using this typeclass;
-- • and, of course, write some Traversable instances.

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- You might notice a similarity between that and the types of fmap and
-- (=<<) (flip bind):
-- fmap :: (a -> b) -> f a -> f b
-- (=<<) :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)

-- mapM is just traverse
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- contrast with
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- Similarly, the type for sequence in GHC versions prior to 7.10 is just
-- a less useful sequenceA:
-- sequence :: Monad m => [m a] -> m [a]
-- contrast with
-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- Again we’re generalizing the list to any Traversable and weakening
-- the Monad requirement to just Applicative.

-- ? So, what’s traversable for?

-- ? Axing tedious code
-- Try to bear with us for a moment and realize that the following is real
-- but also intentionally fake code. That is, one of the authors helped
-- somebody with refactoring their code, and this simplified version is
-- what your author was given. One of the strengths of Haskell is that
-- we can work in terms of types without worry about code that actually
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case mapM decodeFn a of
        (Left err) -> return $ Left err
        (Right res) -> do
            a <- makeIoOnlyObj res
            return $ Right a

-- The objective was to clean up this code. A few things
-- made them suspicious:
-- 1. The use of sequence and map.
-- 2. Manually casing on the result of the sequence and
-- the map.
-- 3. Binding monadically over the Either only to perform
-- another monadic (IO) action inside of that.
-- Here’s what the pipeline function got pared down to
pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)

-- We can make it pointfree if we want to:
