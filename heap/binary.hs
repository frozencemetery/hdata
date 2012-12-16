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
sizes (Both _ l r s) = sizes l && 
                       sizes r && 
                       abs (size' l - size' r) <= 1 && 
                       s == size' l + size' r + 1

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
insert' a (Both b left right s)
  | a < b = insert' b (Both a left right s)
  | size' left > size' right =
    -- insert into right
    Both b left (insert' a right) (s+1)
  | otherwise =
    -- insert into left
    Both b (insert' a left) right (s+1)
insert :: (Eq a, Ord a) => a -> Heap a -> Heap a
insert x Empty = H $ Neither x 1
insert x (H h) = H $ insert' x h

-- delete the minimum element
deleteMin :: (Ord a, Eq a) => Heap a -> (Maybe a, Heap a)
deleteMin Empty = (Nothing, Empty)
deleteMin (H (Neither a 1)) = (Just a, Empty)
deleteMin (H h) =
  let
    dm :: (Ord a, Eq a) => Heap' a -> (a, Heap' a)
    dm (Neither a 1) = undefined -- should never be reached
    dm (Once a l s) = (a, l)
    dm (Both a l r s) =
      if size' l > size' r then
        -- take it from l
        -- this will never be Neither because strict >
        let (e, newl) = dm l
            r' = insert' e r -- rebalance code is redundant
            (q, newr) = dm r'
        in (a, Both q newl newr (s-1))
      else
        -- take it from r
        case r of
          Neither re _ -> -- they're both neither
            let Neither le _ = l
            in (a, Once (min le re) (Neither (max le re) 1) 2)
          _ -> -- neither are neither
            let (e, newr) = dm r
                l' = insert' e l
                (q, newl) = dm l'
            in (a, Both q newl newr (s-1))
  in
    case dm h of (a, ha) -> (Just a, H ha)

-- combine two heaps
meld = undefined

-- is the element in the heap
elem = undefined

-- delete the specified element from the heap
delete = undefined

-- turn a list into a heap
fromList = undefined

-- turns a heap into a list
toList = undefined

-- return the largest element of the heap
maximum = undefined
