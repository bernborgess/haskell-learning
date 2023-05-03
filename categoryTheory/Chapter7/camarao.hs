data Par a = Par a a
instance Functor Par where
    fmap f (Par x y) = Par (f x) (f y)

data Q a b = Q a a b
instance Functor Q where
    fmap f (Q x y z) = Q (f x) (f y) (f z)

data T a b = T a b b
instance Functor T where
    fmap f (T x y z) = T (f x) (f y) (f z)
    
