import qualified Data.Map as Map

memoize :: (Ord a) => (a -> b) -> a -> b
memoize f = let cache = Map.empty in \x ->
    case Map.lookup x cache of
        Just y  -> y
        Nothing -> let y = f x in y `seq` Map.insert x y cache `seq` y
