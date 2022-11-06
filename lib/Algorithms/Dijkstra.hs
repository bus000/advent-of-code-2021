module Algorithms.Dijkstra
    ( dijkstra
    ) where

import qualified Data.List as L
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding((:->)))
import qualified Data.Set as Set

-- Perform Dijkstra's shortest path algorithm from a initial state of type a and
-- a neighbourhood function producing a list of neighbours with their prices p.
-- The function will produce a list of reachable states with the price of
-- reaching that state. The ordering of returned states are cheapest -> most
-- expensive.
dijkstra :: (Ord a, Num p, Ord p)
    -- The initial state.
    => a
    -- Neighbourhood function.
    -> (a -> [(a, p)])
    -- List of states with the price of reaching that state.
    -> [(a, p)]
dijkstra from neighbours = go initialQueue Set.empty
  where
    go queue visited = case PQ.minView queue of
        Nothing -> []
        Just (x :-> price, queue') ->
            (x, price) : go (updateNeighbours queue' visited x price)
                (Set.insert x visited)

    initialQueue = PQ.singleton from 0

    updateNeighbours queue visited x price
        = L.foldl' (setMinPrice price) queue
        . filter (not . (flip Set.member) visited . fst)
        $ neighbours x

    setMinPrice price queue (x, xPrice) =
        PQ.alter (minPrice price xPrice) x queue

    minPrice price xPrice Nothing = Just (price + xPrice)
    minPrice price xPrice (Just oldPrice) =
        Just $ min (price + xPrice) oldPrice
