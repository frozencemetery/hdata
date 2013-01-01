import Data.List

data FH a = FH { minTree :: BT a -- excluded from trees
               , trees :: [BT a]
               } deriving (Show)

data BT a = E | BT { elt :: a
                   , minChild :: BT a -- excluded from children
                   , children :: [BT a]
                   } deriving (Show)

-- O(1)
empty :: FH a
empty = FH { minTree = E, trees = [] }

-- O(1)
singleton :: a -> FH a
singleton a = FH { minTree = BT { elt = a
                                , minChild = E
                                , children = [] 
                                }
                 , trees = []
                 }

-- O(1)
merge :: (Ord a) => FH a -> FH a -> FH a
merge a b =
  case (minTree a, minTree b) of
    (E, _) -> b
    (_, E) -> a
    (a', b') ->
      if elt a' <= elt b' then
        FH { minTree = a', trees = [b'] ++ trees a ++ trees b }
      else
        FH { minTree = b', trees = [a'] ++ trees a ++ trees b }

-- O(1)
insert :: (Ord a) => a -> FH a -> FH a
insert a = merge (singleton a)

-- O(1)
findMin :: FH a -> Maybe a
findMin a =
  case minTree a of
    E -> Nothing
    b -> Just $ elt b

-- O(log n) amortized
deleteMin :: FH a -> (Maybe a, FH a)
deleteMin (FH E _) = (Nothing, empty)
deleteMin (FH mt ts) = undefined

-- O(log n) amortized for single match
delete :: a -> FH a -> (Bool, FH a)
delete = undefined
