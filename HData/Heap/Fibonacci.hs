-- a nontrivial runtime improvement will probably arise out of eliminating the
-- safety of the Maybes and allowing direct calling into the functions
--
-- I expect there's general ricing to be done as well

module HData.Heap.Fibonacci where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST

data FH a = FH { minTree :: BT a -- excluded from trees
               , trees :: [BT a]
               } deriving (Show)

data BT a = E | BT { elt :: a
                   , children :: [BT a]
                   , rank :: Int
                   } deriving (Show, Eq)

-- O(1) non-amortized
empty :: FH a
empty = FH { minTree = E, trees = [] }

-- O(1) non-amortized
singleton :: a -> FH a
singleton a = FH { minTree = BT { elt = a
                                , children = []
                                , rank = 1
                                }
                 , trees = []
                 }

-- O(1) non-amortized
merge :: (Ord a) => FH a -> FH a -> FH a
merge a b =
  case (minTree a, minTree b) of
    (E, _) -> b
    (_, E) -> a
    (a', b') ->
      if elt a' <= elt b' then
        FH { minTree = a', trees = b' : trees a ++ trees b }
      else
        FH { minTree = b', trees = a' : trees a ++ trees b }

-- O(1) non-amortized
insert :: (Ord a) => a -> FH a -> FH a
insert a = merge (singleton a)

-- O(1) non-amortized
findMin :: FH a -> Maybe a
findMin a =
  case minTree a of
    E -> Nothing
    b -> Just $ elt b

-- O(log n) amortized
-- logic lifted from liuxinyu95 (under GPLv3)
deleteMin :: (Ord a) => FH a -> (Maybe a, FH a)
deleteMin (FH E _) = (Nothing, empty)
deleteMin (FH (BT e [] _) []) = (Just e, empty)
deleteMin (FH mt ts) =
  let ret = elt mt
      ts' = children mt ++ ts
      splitMin :: (Ord a) => [BT a] -> (BT a, [BT a])
      splitMin [x] = (x, [])
      splitMin (x:xs) =
        let (r, xs') = splitMin xs
            (l, s) = if elt x < elt r then (r, x) else (x, r)
        in (s, l:xs')
      (minE, circE) = splitMin ts'
      coalesce :: (Ord a) => [BT a] -> BT a -> [BT a]
      coalesce [] x = [x]
      coalesce (x:xs) cur
        | rank x == rank cur = coalesce xs $
          if elt x < elt cur then
            BT { elt = elt x
               , children = cur : children x
               , rank = 1 + rank x
               }
          else
            BT { elt = elt cur
               , children = x : children cur
               , rank = 1 + rank x
               }
        | rank cur < rank x = cur:x:xs
        | otherwise = x : coalesce xs cur
  in (Just ret, FH { minTree = minE
                   , trees = foldl coalesce [] circE
                   }
     )

-- O(log n) amortized (assumes single match)
delete :: a -> FH a -> (Bool, FH a)
delete _ (FH E _) = (False, empty)
delete _ _ = undefined

main = undefined -- TODO kill this
