import Data.Set (Set)
import Data.Set qualified as Set

addListToSet :: Ord a => Set a -> [a] -> Set a
addListToSet = foldr Set.insert
