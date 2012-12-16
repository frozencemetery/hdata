-- TODO this uses AVL explain why

import Data.Maybe

type Size = Int

data Heap a = E | H a (Heap a) (Heap a) Size

-- verify heap integrity
sizes :: (Eq a, Ord a) => Heap a -> Bool
sizes E = True
sizes (H _ l r s) = sizes l &&
                    sizes r &&
                    abs (size l - size r) <= 1 &&
                    s == size l + size r + 1

structure :: (Eq a, Ord a) => Heap a -> Bool
structure E = True
structure (H e l r _) =
  let
    lh = structure l && if isEmpty l then True else e <= fromJust $ minimum l
    lh = structure r && if isEmpty r then True else e <= fromJust $ minimum r
  in lh && rh

isGood :: (Eq a, Ord a) => Heap a -> Bool
isGood x = sizes x && structure x

-- gets the smallest element
minimum :: (Eq a, Ord a) => Heap a -> Maybe a
minimum E = Nothing
minimum (H x _ _ _) = x

-- is the heap empty
isEmpty :: (Eq a, Ord a) => Heap a -> Bool
isEmpty E = True
isEmpty _ = False

-- number of elements contained
size :: (Eq a, Ord a) => Heap a -> Size
size E = 0
size (H _ _ _ s) = s

-- create a new empty heap
empty :: (Eq a, Ord a) => Heap a
empty = E

-- create a heap of one element
singleton :: (Eq a, Ord a) => a -> Heap a
singleton x = H x E E 1

-- insert into the heap
insert :: (Eq a, Ord a) => a -> Heap a -> Heap a
insert a E = H a E E a
insert a (H e l r s)
  | a < e = insert e (H a l r s)
  | size l > size r = H left (insert a right) (s+1)
  | otherwise = H (insert a left) right (s+1)

-- delete the minimum element
deleteMin :: (Ord a, Eq a) => Heap a -> (Maybe a, Heap a)
deleteMin E = (Nothing, Empty)
deleteMin (H e l r s)
  | isEmpty l && isEmpty r = (Just e, E)
  | size l > size r =
    let (Just x, newl) = deleteMin l
    in (Just e, H x newl r (s-1))
  | otherwise =
    let (Just x, newr) = deleteMin r
    in (Just e, H x l newr (s-1))

-- is the element in the heap
elem :: (Ord a, Eq a) => a -> Heap a -> Bool
elem _ E = False
elem x (H e l r s) =
  let ch = x == e
      lh = if x >= minimum l then elem x l else False
      rh = if x >= minimum r then elem x r else False
  in ch || rh || lh

-- delete all occurrences of the specified element from the heap
-- if there are multiple occurrences of an element in the heap, the shape
-- property is not guaranteed to be preserved.  However, insert and delete
-- operations do not assume this property and it will approach the shape
-- property over successive applications of both.
-- this is currently O(n).  On the TODO list is making it more efficient O(n),
-- which will happen through short circuiting the traversal harder.
deleteAll :: (Ord a, Eq a) => a -> Heap a -> (Bool, Heap a)
deleteAll _ E = (False, E)
deleteAll x (H e l r s) =
  if x == e then
    let (_, r) = deleteAll x $ snd $ deleteMin (H e l r s)
    in (True, r)
  else
    let (lb, newl) = deleteAll x l
        (rb, newr) = deleteAll x r
    in (lb || rb, H e newl newr (size newl + size newr))

-- combine two heaps
meld = undefined

-- turn a list into a heap
fromList = undefined

-- turns a heap into a list
toList = undefined

-- return the largest element of the heap
maximum = undefined
