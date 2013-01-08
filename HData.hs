module HData where

import Data.List
import Data.Maybe

import qualified HData.Heap as H
import qualified HData.PriorityQueue as PQ

import qualified Data.Set as S

tabulate :: [a] -> [(Int, a)]
tabulate = zip [1..]

pfac :: (Enum a, Num a) => a -> a -> a
pfac low high = product [low..high]

factorial :: (Enum a, Num a) => a -> a
factorial = pfac 1

choose :: (Integral a) => a -> a -> a
n `choose` k = (factorial n) `div` ((factorial k) * (factorial $ n-k))

-- works only on sorted
-- be sure this does what you want
collect :: (Eq a) => [a] -> [(a, Int)]
collect [] = []
collect xs = map (\x -> (head(x), length(x))) $ group xs

-- fast and works on all values
fib :: (Num a, Eq a) => a -> a
fib =
  let incfib :: (Num a, Eq a) => (a, a) -> (a, a)
      incfib (curr, prev) = (curr+prev, curr)
      fib' :: (Num a, Eq a) => a -> (a, a)
      fib' 0 = (1, 0)
      fib' n =
        let enone = fib' $ n-1
        in incfib enone
  in fst . fib'

-- fast but innacurate for large values (i.e., >= 75)
fibApprox :: (Integral a) => a -> a
fibApprox i =
  let s5 = sqrt 5 :: Double
      fracpos = ((1 + sqrt 5) / 2) ^ (i+1) :: Double
      fracneg = ((1 - sqrt 5) / 2) ^ (i+1) :: Double
      internal = fracpos - fracneg :: Double
      base = 1 / s5 :: Double
  in floor $ 0.5 + base * internal

-- this code is based on code from the paper
-- "The Genuine Sieve of Eratosthenes"
-- by Melissa E. O'Neill
sieve :: (Num a, Ord a) => [a] -> [a]
sieve [] = []
sieve (x:xs) = 
  let insertprime :: (Num a, Ord a) => a -> [a] -> PQ.PQ a [a] -> PQ.PQ a [a]
      insertprime p xs table = PQ.insert (p*p) (map ( *p) xs) table
      sieve' :: (Num a, Ord a) => [a] -> PQ.PQ a [a] -> [a]
      sieve' [] table = []
      sieve' (x:xs) table =
        let nextComposite = fromJust $ PQ.minKey table
            adjust table = -- TODO type of this
              let (n, n':ns) = fromJust $ PQ.minKeyValue table
              in if n <= x then -- closures
                   adjust (PQ.deleteMinAndInsert n' ns table)
                 else
                   table
        in if nextComposite <= x then
             sieve' xs (adjust table)
           else
             x : sieve' xs (insertprime x xs table)
  in x : sieve' xs (insertprime x xs PQ.empty)
primes = sieve [2..] :: [Int]
primes' = sieve [2..] :: [Integer]
isPrime :: Int -> Bool
isPrime n =
  let p = dropWhile (< n) primes
  in n == head p

lpf :: (Num a, Ord a, Integral a) => a -> [a]
lpf =
  let walkpf :: (Num a, Ord a, Integral a) => a -> a -> [a]
      walkpf cur tgt
        | cur > tgt = []
        | tgt `mod` cur == 0 = cur : walkpf cur (tgt `div` cur)
        | otherwise = walkpf (cur+1) tgt
  in walkpf 2

lf :: (Num a, Ord a, Integral a) => a -> [a]
lf 1 = [1]
lf i = 
  let pfs = group $ lpf i
      combinate :: (Num a, Ord a, Integral a) => [[a]] -> [a]
      combinate [] = [1]
      combinate (xl:xls) =
        let xls' = combinate xls
            xlm = map product $ tails xl
            xlm' = concatMap (\lst -> map (* lst) xls') xlm
        in xlm'
  in init $ sort $ combinate pfs

cmpfsts :: (Ord a) => (a,b) -> (a,b) -> Ordering
cmpfsts (a1, _) (a2, _) = compare a1 a2

cmpsnds :: (Ord b) => (a,b) -> (a,b) -> Ordering
cmpsnds (_, b1) (_, b2) = compare b1 b2

rotations :: [a] -> [[a]]
rotations xs =
  let r :: [a] -> [a]
      r xs = (tail xs) ++ [head xs]
      rotate :: Int -> [a] -> [[a]] -> [[a]]
      rotate 0 _ acc = acc
      rotate n xs acc = rotate (n-1) (r xs) (xs:acc)
  in rotate (length xs) xs []
