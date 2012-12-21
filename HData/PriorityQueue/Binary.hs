-- this uses a binary heap as the basis for a priority queue

module HData.PriorityQueue.Binary where

import HData.Heap.Binary as H

type PQ a b = H.Heap (a,b)

empty :: (Ord a, Ord b) => PQ a b
empty = H.empty

minKey :: (Ord a, Ord b) => PQ a b -> Maybe a
minKey x = case H.minelt x of
             Nothing -> Nothing
             Just (q, _) -> Just q

minKeyValue :: (Ord a, Ord b) => PQ a b -> Maybe b
minKeyValue x = case H.minelt x of
                  Nothing -> Nothing
                  Just (_, q) -> Just q

insert :: (Ord a, Ord b) => a -> b -> PQ a b -> PQ a b
insert k v = H.insert (k, v)

deleteMinAndInsert :: (Ord a, Ord b) => a -> b -> PQ a b -> PQ a b
deleteMinAndInsert k v = H.deleteMinAndInsert (k, v)
