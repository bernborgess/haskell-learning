{-# LANGUAGE TupleSections #-}

module Tree (tree) where

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

-- m a -> Int -> (m Int, Int)
-- m a -> Int -> Writer (m Int,Int)
-- Reader a (Writer (m Int) Int)

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s

instance Functor (State s) where
    fmap f (State g) = State $ \s ->
        let (a, s') = g s
         in (f a, s')

instance Applicative (State s) where
    pure a = State (a,)

    (State sab) <*> (State sa) = State $ \s ->
        let (f, s') = sab s
            (a, s'') = sa s'
         in (f a, s'')

sInc :: State Int Int
sInc = State (\n -> (n, n + 1))

alabel :: Tree a -> State Int (Tree Int)
alabel (Leaf _) = Leaf <$> sInc
alabel (Node l r) = Node <$> alabel l <*> alabel r
