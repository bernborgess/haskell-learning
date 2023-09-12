data Node a = Node
    { el :: a
    , before :: Maybe (Node a)
    }
    deriving (Eq, Show)

instance Functor Node where
    fmap f (Node{el, before}) =
        Node{el = f el, before = fmap f <$> before}

instance Applicative Node where
    pure a = Node a Nothing

    (Node{el = ab, before = (Just x)}) <*> (Node{el = a, before = (Just y)}) =
        Node (ab a) (Just $ x <*> y)
    (Node{el = ab}) <*> (Node{el = a}) = Node (ab a) Nothing

instance Monad Node where
    (Node{el, before = Nothing}) >>= anb = anb el
    (Node{el, before = (Just na)}) >>= anb =
        let (Node{el = b}) = anb el
            nb = na >>= anb
         in Node b (Just nb)

newtype Stack a = Stack {tip :: Maybe (Node a)}

instance Functor Stack where
    fmap f (Stack{tip}) = Stack{tip = fmap f <$> tip}

instance Applicative Stack where
    pure a = Stack{tip = Just $ pure a}

    (Stack{tip = (Just nab)}) <*> (Stack{tip = (Just na)}) =
        Stack (Just $ nab <*> na)