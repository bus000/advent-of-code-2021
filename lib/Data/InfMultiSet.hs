{- A multiset implementation that allows infinitely many of each element.
 - I.e the backing counter is an Integer and not an Int. -}
module Data.InfMultiSet where

import qualified Data.List as L
import qualified Data.Map as Map

{- A multiset is just a map from key to count. -}
newtype MultiSet a = MultiSet
    { _theMap :: (Map.Map a Integer)
    } deriving (Show, Eq, Ord)

{- The empty set. -}
empty :: MultiSet a
empty = MultiSet Map.empty

{- Convert list of elements to multiset. -}
fromList :: Ord a => [a] -> MultiSet a
fromList = MultiSet . Map.fromListWith (+) . map (\x -> (x, 1))

{- Take the union of two multisets. -}
union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet ms1) (MultiSet ms2) = MultiSet $ Map.unionWith (+) ms1 ms2

{- Take the unions of many multisets. -}
unions :: Ord a => [MultiSet a] -> MultiSet a
unions = L.foldl' union empty

{- Convert to list of element and count of that element. -}
toOccurList :: MultiSet a -> [(a, Integer)]
toOccurList = Map.toList . _theMap

{- Remove element from set. Only remove the element if count reaches 0. -}
delete :: Ord a => a -> MultiSet a -> MultiSet a
delete key (MultiSet ms) = MultiSet $ Map.update f key ms
  where
    f 1 = Nothing
    f n = Just $ n - 1

{- Delete all the elements from the multiset. -}
deleteMany :: Ord a => [a] -> MultiSet a -> MultiSet a
deleteMany keys ms = L.foldl' (flip delete) ms keys
