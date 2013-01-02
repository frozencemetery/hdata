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
deleteMin :: (Ord a) => FH a -> (Maybe a, FH a)
deleteMin (FH E _) = (Nothing, empty)
deleteMin (FH (BT e [] _) []) = (Just e, empty)
deleteMin (FH mt ts) = runST $ do
  let ret = elt mt
  let ats = children mt ++ ts
  let limit = rank $ maximumBy (\a b -> compare (rank a) (rank b)) ats
  arr <- newArray (1, limit + length ats) Nothing
      :: ST s (STArray s Int (Maybe (BT a)))
  forM_ ats $ \x -> do
    melt <- readArray arr (rank x)
    case melt of 
      Nothing -> writeArray arr (rank x) (Just x)
      Just a -> (writeArray arr (rank x) Nothing) >> 
                (writeArray arr (1 + rank x) 
                              (Just $ if elt a < elt x then
                                        BT { elt = elt a
                                           , children = x : children a 
                                           , rank = 1 + rank x 
                                           }
                                      else
                                        BT { elt = elt x
                                           , children = a : children x
                                           , rank = 1 + rank x
                                           }
                              )
                )
  elems <- getElems arr
  let es = map fromJust $ filter isJust elems
  let minE = minimumBy (\x y -> compare (elt x) (elt y)) es
  let (_, childE) = span (== minE) es
  return $ (Just ret, FH { minTree = minE
                         , trees = childE
                         }
           )

-- O(log n) amortized (assumes single match)
delete :: a -> FH a -> (Bool, FH a)
delete _ (FH E _) = (False, empty)
delete _ _ = undefined

main = undefined -- TODO kill this
