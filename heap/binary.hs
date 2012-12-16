-- TODO this uses AVL explain why

import Data.Maybe

type Size = Int

data Heap a = Empty
            | H (Heap' a)
              deriving (Show) -- debug only

data Heap' a = Neither a Size
             | Once a (Heap' a) Size -- always left
             | Both a (Heap' a) (Heap' a) Size
               deriving (Show) -- debug only

-- verify heap integrity
sizes :: (Eq a, Ord a) => Heap' a -> Bool
sizes (Neither _ s) = s == 1
sizes (Once _ l s) = sizes l && s == (1 + size' l)
sizes (Both _ l r s) = sizes l && sizes r && abs (size' l - size' r) <= 1 && s == size' l + size' r + 1

structure :: (Eq a, Ord a) => Heap' a -> Bool
structure (Neither _ _) = True
structure (Once e l _) = case l of Neither x _ -> e <= x
                                   Once x _ _ -> e <= x && structure l
                                   Both x _ _ _ -> e <= x && structure l
structure (Both e l r _) =
  let
    lh = case l of Neither x _ -> e <= x
                   Once x _ _ -> e <= x && structure l
                   Both x _ _ _ -> e <= x && structure l
    rh = case r of Neither x _ -> e <= x
                   Once x _ _ -> e <= x && structure r
                   Both x _ _ _ -> e <= x && structure r
  in
    lh && rh

isGood :: (Eq a, Ord a) => Heap a -> Bool
isGood Empty = True
isGood (H h) = sizes h && structure h

-- is the heap empty
isEmpty :: (Eq a, Ord a) => Heap a -> Bool
isEmpty Empty = True
isEmpty (H _) = False

-- it's nice to have size' for internal use
size' :: (Eq a, Ord a) => Heap' a -> Size
size' h = case h of Neither _ s -> s
                    Once _ _ s -> s
                    Both _ _ _ s -> s
-- number of elements contained
size :: (Eq a, Ord a) => Heap a -> Size
size Empty = 0
size (H h) = size' h

-- create a new empty heap
empty :: (Eq a, Ord a) => Heap a
empty = Empty

-- create a heap of one element
singleton :: (Eq a, Ord a) => a -> Heap a
singleton x = H (Neither x 1)

-- return the smallest element of the heap
minimum :: (Eq a, Ord a) => Heap a -> Maybe a
minimum Empty = Nothing
minimum (H h) = Just $ case h of Neither x _ -> x
                                 Once x _ _ -> x
                                 Both x _ _ _ -> x

-- insert into the heap
insert :: (Eq a, Ord a) => a -> Heap a -> Heap a
insert x Empty = H $ Neither x 1
insert x (H h) = H $
  let
    insert' :: (Eq a, Ord a) => a -> Heap' a -> Heap' a
    insert' a (Neither b s) =
      if a <= b then
        Once a (Neither b 1) 2
      else
        Once b (Neither a 1) 2
    insert' a (Once b bh s) =
      if a <= b then
        Both a bh (Neither b 1) (s+1)
      else
        Both b bh (Neither a 1) (s+1)
    insert' a (Both b left right s) = 
      if a < b then
        insert' b (Both a left right s)
      else if (size' left) > (size' right) then
        -- insert into right
        Both b left (insert' a right) (s+1)
      else
        -- insert into left
        Both b (insert' a left) right (s+1)
  in
    insert' x h

-- combine two heaps
meld = undefined

-- is the element in the heap
elem = undefined

-- delete the minimum element
deleteMin = undefined

-- delete the specified element from the heap
delete = undefined

-- turn a list into a heap
fromList = undefined

-- turns a heap into a list
toList = undefined

-- return the largest element of the heap
maximum = undefined
