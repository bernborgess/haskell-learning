import Control.Monad ((>=>))

-- Write-only

isEven :: Int -> (Bool, String)
isEven x = (even x, " even ")

not' :: Bool -> (Bool, String)
not' b = (not b, " not' ")

isOdd :: Int -> (Bool, String)
isOdd x =
    let (b1, trace1) = isEven x
        (b2, trace2) = not' b1
     in (b2, trace1 ++ trace2)

data Writer w a = Writer a w
    deriving (Show)

runWriter :: Writer w a -> (a, w)
runWriter (Writer a w) = (a, w)

tell :: w -> Writer w ()
tell w = Writer () w

instance Functor (Writer w) where
    -- fmap :: (a->b) -> Writer w a -> Writer w b
    fmap f (Writer a w) = Writer (f a) w

instance Monoid w => Applicative (Writer w) where
    -- pure a -> Writer w a
    pure a = Writer a mempty

    -- (<*>) :: Writer w (a->b) -> Writer w a -> Writer w b
    (Writer f m1) <*> (Writer a m2) = Writer (f a) (m1 <> m2)

instance Monoid w => Monad (Writer w) where
    -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (Writer a m) >>= f =
        let (b, m') = runWriter $ f a
         in Writer b (m <> m')

-- isOdd 3
-- >> (True," isEven not'")

isEvenW :: Int -> Writer String Bool
isEvenW x = do
    tell " even x "
    return (even x)

notW :: Bool -> Writer String Bool
notW b = do
    tell " not "
    return (not b)

isOddW :: Int -> Writer String Bool
-- isOddW x = do
--     b <- isEvenW x
--     notW b
isOddW = isEvenW >=> notW