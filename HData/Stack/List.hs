module HData.Stack.List where

import Data.List

data Stack a = S [a] deriving (Eq)

push :: Stack a -> a -> Stack a
push (S s) e = S $ e:s

pop :: Stack a -> (Maybe a, Stack a)
pop (S []) = (Nothing, S [])
pop (S (x:xs)) = (Just x, S xs)

pop_unsafe :: Stack a -> (a, Stack a)
pop_unsafe (S (x:xs)) = (x, S xs)

empty :: Stack a
empty = S []

isEmpty :: Stack a -> Bool
isEmpty (S xs) = length xs == 0
