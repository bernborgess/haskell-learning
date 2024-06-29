-- ? Monads

-- >>= (bind)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Can extract a value from a monad

-- Just 1 >>= (\x -> Just x)
-- ==> Just 1

-- Nothing >>= (\x -> Just x)
-- ==> Nothing

maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x+y)

-- ghci> maybeadd Nothing 1
-- Nothing
-- ghci> maybeadd (Just 1) 1
-- Just 2

maybeaddmaybe :: Num b => Maybe b -> Maybe b -> Maybe b
maybeaddmaybe mx my = 
  mx >>= (\x -> my >>= (\y -> Just $ x+y))

-- ghci> maybeaddmaybe Nothing (Just 1)
-- Nothing
-- ghci> maybeaddmaybe (Just 2) (Just 1)
-- Just 3
-- ghci> maybeaddmaybe (Just 2) Nothing
-- Nothing

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my =
  mx >>= (\x -> my >>= (\y -> return $ x+y))


monaddDo :: (Monad m, Num b) => m b -> m b -> m b
monaddDo mx my = do
  x <- mx
  y <- my
  return $ x + y

-- ? Maybe Monad
instance Monad Maybe where
  m >>= f = case m of
    Nothing -> Nothing
    Just x -> f x
  return v = Just v


-- ! fail :: String -> m a
-- (define error procedure)


-- ? >>
-- (>>) :: Monad m => m a
-- m >> n = m >>= \_ -> n

-- ghci> Nothing >> Just 1
-- Nothing
-- ghci> Just 1 >> Just 2
-- Just 2
-- ghci> Just 1 >> Nothing
-- Nothing



-- ? Monad Laws

-- * Left Identity
--  return a >>= k === k a

-- * Right Identity
--  m >>= return === m

-- * Associativity
--  m >>= (\x -> k x >>= h) === (m >>= k) >>= h
