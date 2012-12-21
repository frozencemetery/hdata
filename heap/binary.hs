-- We use a slightly modified version of the normal binary heap property: the
-- left and right children at each node must differ by at most one, with the
-- left being the larger.  The complexity is identical.

import Data.Maybe

type Size = Int

data Heap a = E | H a (Heap a) (Heap a) Size

instance (Show a) => Show (Heap a) where
  show E = "EMPTY"
  show a =
    let pss :: Integer -> String -> String
        pss 0 acc = acc
        pss i acc = pss (i-1) $ ' ' : acc
        s' :: (Show a) => Heap a -> Integer -> String
        s' E i = (pss i "") ++ "EMPTY"
        s' (H e l r s) i =
          let lh = s' l $ i+2
              rh = s' r $ i+2
              sp = pss i ""
          in sp ++ "(" ++ show e ++ ") :: " ++ show s ++ "\n" ++ lh ++ "\n" ++ rh
    in s' a 0

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
  let lh = structure l && isEmpty l || e <= fromJust (minelt l)
      rh = structure r && isEmpty r || e <= fromJust (minelt r)
  in lh && rh

isGood :: (Eq a, Ord a) => Heap a -> Bool
isGood x = sizes x && structure x

-- gets the smallest element
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

-- create a heap of one element
singleton :: (Eq a, Ord a) => a -> Heap a
singleton x = H x E E 1

-- insert into the heap
insert :: (Eq a, Ord a) => a -> Heap a -> Heap a
insert a E = H a E E 1
insert a (H e l r s)
  | a < e = insert e (H a l r s)
  | size l > size r = H e l (insert a r) (s+1)
  | otherwise = H e (insert a l) r (s+1)

-- delete the minimum element
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
      lh = x >= fromMaybe x (minelt l) && elt x l
      rh = x >= fromMaybe x (minelt r) && elt x r
  in ch || rh || lh

-- delete all occurrences of the specified eltent from the heap
-- this kinda knocks the hell out of the shape property
-- O(log n) for a single occurrence, up to O(n log n)
deleteAll :: (Ord a, Eq a) => a -> Heap a -> (Bool, Heap a)
deleteAll _ E = (False, E)
deleteAll x (H e l r s) =
  let resize :: (Ord a, Eq a) => Heap a -> Heap a
      resize (H e l r s)
        | size l > (size r + 1) =
          let (Just e', l') = deleteMin l
              r' = insert e' r
          in resize $ H e l' r' s
        | size r > (size l + 1) =
          let (Just e', r') = deleteMin r
              l' = insert e' l
          in resize $ H e l' r' s
        | otherwise =
          H e l r s
  in if e == x then
       let (b, nh) = deleteAll x $ snd $ deleteMin $ H e l r s
           nh' = if b then resize nh else nh
       in (True, nh')
     else
       let (lb, lh) = if size l == 0 || (fromJust $ minelt l) <= x then
                        deleteAll x l
                      else
                        (False, l)
           (rb, rh) = if size r == 0 || (fromJust $ minelt r) <= x then
                        deleteAll x r
                      else
                        (False, r)
           h = H e lh rh (size lh + size rh + 1)
           h' = if lb || rb then resize h else h
       in (lb || rb, h')


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
toRlist E = []
toRlist h =
  let tl :: (Ord a, Eq a) => [Heap a] -> [a] -> [a]
      tl [] acc = acc
      tl hl acc =
        let next = minimum $ mapMaybe minelt hl
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
-- this should be O(n) unless I did it wrong
fromList :: (Ord a, Eq a) => [a] -> Heap a
fromList =
  let
    fl :: (Ord a, Eq a) => [a] -> Heap a
    fl [] = E
    fl [e] = H e E E 1
    fl [a,b] = H a (H b E E 1) E 2
    fl (x:xs) =
      let len = (length xs + 1) `div` 2
          (lh, rh) = splitAt len xs
          (l, r) = (fl lh, fl rh)
      in H x l r (size l + size r + 1)
    fs :: (Ord a, Eq a) => Heap a -> Heap a
    fs E = E
    fs (H e E E s) = (H e E E 1)
    fs (H e E l s) = fs (H e l E s) -- bad
    fs (H e l r s) =
      let (H le ll lr ls) = fs l
          e' = min e le
          le' = max e le
      in case fs r of
           E -> H e' (H le' ll lr ls) E (ls+1)
           H re rl rr rs ->
             let e'' = min e' re
                 re' = max e' re
             in H e'' (H re' rl rr rs) (H le' ll lr ls) (rs + ls + 1)
  in fs . fl

-- combine two heaps
-- this doesn't care about uniqueness of elts
-- O(n) though which is nice
meld :: (Ord a, Eq a) => Heap a -> Heap a -> Heap a
meld a b =
  if size a > size b then
    meld b a
  else
    fromList $ toUlist a ++ toUlist b
