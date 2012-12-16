-- TODO this uses AVL explain why

import Data.Maybe

type Size = Int

data Heap a = E | H a (Heap a) (Heap a) Size deriving (Show)

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
    lh = structure l && if isEmpty l then True else e <= fromJust (minelt l)
    rh = structure r && if isEmpty r then True else e <= fromJust (minelt r)
  in lh && rh

isGood :: (Eq a, Ord a) => Heap a -> Bool
isGood x = sizes x && structure x

-- gets the smallest eltent
minelt :: (Eq a, Ord a) => Heap a -> Maybe a
minelt E = Nothing
minelt (H x _ _ _) = Just x

-- is the heap empty
isEmpty :: (Eq a, Ord a) => Heap a -> Bool
isEmpty E = True
isEmpty _ = False

-- number of eltents contained
size :: (Eq a, Ord a) => Heap a -> Size
size E = 0
size (H _ _ _ s) = s

-- create a new empty heap
empty :: (Eq a, Ord a) => Heap a
empty = E

-- create a heap of one eltent
singleton :: (Eq a, Ord a) => a -> Heap a
singleton x = H x E E 1

-- insert into the heap
insert :: (Eq a, Ord a) => a -> Heap a -> Heap a
insert a E = H a E E 1
insert a (H e l r s)
  | a < e = insert e (H a l r s)
  | size l > size r = H e l (insert a r) (s+1)
  | otherwise = H e (insert a l) r (s+1)

-- delete the minelt eltent
deleteMin :: (Ord a, Eq a) => Heap a -> (Maybe a, Heap a)
deleteMin E = (Nothing, E)
deleteMin (H e l r s)
  | isEmpty l && isEmpty r = (Just e, E)
  | size l > size r =
    let (Just x, newl) = deleteMin l
        newr = insert x r
        (Just q, newr') = deleteMin newr
    in (Just e, H q newl newr' (s-1))
  | otherwise =
    let (Just x, newr) = deleteMin r
        newl = insert x l
        (Just q, newl') = deleteMin newl
    in (Just e, H q newl' newr (s-1))

-- is the element in the heap
elt :: (Ord a, Eq a) => a -> Heap a -> Bool
elt _ E = False
elt x (H e l r s) =
  let ch = x == e
      lh = if x >= fromMaybe x (minelt l) then elt x l else False
      rh = if x >= fromMaybe x (minelt r) then elt x r else False
  in ch || rh || lh

-- delete all occurrences of the specified eltent from the heap
-- if there are multiple occurrences of an eltent in the heap, the shape
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

-- return the largest element of the heap
-- this is an O(n) traversal
maxelt :: (Ord a, Eq a) => Heap a -> Maybe a
maxelt E = Nothing
maxelt (H e l r s) =
  let lm = fromMaybe e $ maxelt l
      rm = fromMaybe e $ maxelt r
  in Just $ max e $ max lm rm

-- turns a heap into a sorted list
-- O(n log n), but better than doing it and then qsorting it
-- in fact, it happens at the speed of heapsort
-- funny thing about that
toList :: (Ord a, Eq a) => Heap a -> [a]
toList = reverse . toRlist

-- turns a heap into a reverse-sorted list
-- see note at toList
-- will be faster than toList because it does not reverse
toRlist :: (Ord a, Eq a) => Heap a -> [a]
toRlist h =
  let tl :: (Ord a, Eq a) => [Heap a] -> [a] -> [a]
      tl [] acc = acc
      tl hl acc =
        let next = minimum $ catMaybes $ map minelt hl
            tv :: (Ord a, Eq a) => [Heap a] -> a -> [Heap a]
            tv [] _ = []
            tv (E:xs) e = tv xs e
            tv ((H e l r s):xs) x = 
              if e == x then 
                tv (l : r : xs) x 
              else (H e l r s) : tv xs x
            newhl = tv hl next
        in tl newhl (next : acc)
  in tl [h] []

-- turns a heap into an unordered list
-- this is O(n)
toUlist :: (Ord a, Eq a) => Heap a -> [a]
toUlist h =
  let tl :: (Ord a, Eq a) => [Heap a] -> [a] -> [a]
      tl [] acc = acc
      tl (E:xs) acc = tl xs acc
      tl ((H e l r s):xs) acc = tl (l : r : xs) (e : acc)
  in tl [h] []

-- turn a list into a heap
-- O(n), so better than mapping insert over your list
-- problem is, I think functional insertion even into bst is O(log n) per elt
fromList = undefined

-- combine two heaps
-- if fromList is O(n), and toList is O(n), then this is easy
meld = undefined
