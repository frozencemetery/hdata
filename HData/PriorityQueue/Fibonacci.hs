-- this uses a fibonacci heap as the basis for a priority queue

module HData.PriorityQueue.Fibonacci where

import HData.Heap.Fibonacci as H

type PQ key value = H.FH (key, value)

empty :: (Ord a, Ord b) => PQ a b
empty = H.empty

minKey :: (Ord a, Ord b) => PQ a b -> Maybe a
minKey h = do elt <- H.findMin h
              return $ fst elt

minKeyValue :: (Ord k, Ord v) => PQ k v -> Maybe (k, v)
minKeyValue = H.findMin

insert :: (Ord a, Ord b) => a -> b -> PQ a b -> PQ a b
insert k v = H.insert (k, v)

deleteMinAndInsert :: (Ord a, Ord b) => a -> b -> PQ a b -> PQ a b
deleteMinAndInsert k v h =
  let h' = snd $ deleteMin h
  in H.insert (k, v) h'

