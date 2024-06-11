{- 
  
  Euler #14 in Curry

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following 
  sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1) 
  contains 
  10 terms. Although it has not been proved yet (Collatz Problem), it is 
  thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  This is - as expected - very slow. 
  In fibonacci.curry I tested memoization but that was too slow (compared
  to the fiblist version (the one used in euler2.curry).

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Function(fix)
import Debug.Trace
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Memoize

-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD


-- collatz n | n `mod` 2 == 0 = n `div` 2
-- collatz n | n `mod` 2 == 1 = (3*n) + 1
collatz :: Integral a => a -> a
collatz n = if n `mod` 2 == 0 then
              n `div` 2
            else
              (3*n) + 1
              
collatzSeq :: Integral a => a -> [a]
collatzSeq n = takeWhile (>1) $ iterate collatz n

-- Brute force
-- PAKCS: 40min14.404s
-- The Haskell version takes 144.00s
-- KICS2 is much faster: 1.19+1.60+37.75s!!!
euler14a :: (Int,Int)
euler14a = maximum $ map (\n -> (length $ collatzSeq n,n) ) [2..1000000]


--
--
-- Based on
-- https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html
-- Same as collatz but with an extra first argument f
-- Much slower
collatz2 :: Integral b => a -> b -> b
collatz2 _ n = if n `mod` 2 == 0 then
              n `div` 2
            else
              (3*n) + 1

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

collatzMemo :: Int -> Int
collatzMemo = fix (memoize . collatz2)
               
-- collatzSeq :: Integral a => a -> [a]
collatzSeqMemo :: Int -> [Int]
collatzSeqMemo n = takeWhile (>1) $ iterate collatzMemo n


-- for 2..100, it takes 6.4s, using the unmemoized version take 80s!
-- so I skip to test this with larger value
-- But KiCS2 might be faster
euler14b :: (Int,Int)
euler14b = maximum $ map (\n -> (length $ collatzSeqMemo n,n) ) [2..1000000]


--
-- Using recursion with accumulator
--
collatzLen3 n len
  | n == 1 = len
  | otherwise = if n `mod` 2 == 0 then
                          collatzLen3 (n `div` 2) (len+1)
                      else
                          collatzLen3 ((n*3)+1) (len+1)

euler14c = maximum [ (c, n) | n <- [1..1000000], let c = collatzLen3 n 1]


-- update the value in position i in list xs to new
update xs i new = a ++ [new] ++ b
                  where (a,b) = splitAt (i-1) xs


--
-- Using getOrUpdate from my Memoize module.
-- (See fibonacci.curry for a similar problem.)
-- Nope, too slow!
--
-- stateMemoCollatz :: Int -> State (Map.Map Int Int) Int
stateMemoCollatz n
  | n == 1         = return 1
  | n `mod` 2 == 0 = do
                       n2 <- (getOrUpdate (n `div` 2) (stateMemoCollatz (n `div` 2)))
                       return (n2 + 1)
  | otherwise      = do
                       n2 <- (getOrUpdate ((3*n) + 1) (stateMemoCollatz ((3*n) + 1)))
                       return (n2 + 1)

-- lazyMemoCollatz :: Int -> Int
lazyMemoCollatz = (fmap collatz [0 ..] !!)
  where
    collatz n
      | n == 1         = 1
      | n `mod` 2 == 0 = 1 + (lazyMemoCollatz (n `div` 2))
      | otherwise      = 1 + (lazyMemoCollatz ((3*n)+1))

euler14d :: (Num a, Ord a) => (a, Int)
euler14d  = maximum $ map (\n -> (lazyMemoCollatz n,n) ) [2..1000000]

--
-- As euler14a but only odd integers (since odd numbers are the one that grow).
--
euler14e :: (Int,Int)
euler14e = maximum $ map (\n -> (length $ collatzSeq n,n) ) [3,5..1000000]


--
-- Pure Data.Map approach: TODO
--
-- euler14f' m n
--   | n > 1000000 = (n,m)
--   | otherwise   =  (n, euler14f' m' (n+1))
--                    where
--                      n' = if n `mod` 2 then n `div` 2 else (3*n) + 1
--                      m' = if Map.member n m then
--                         1 + (Map.lookup n m)
--                      else
--                        euler14f' Map.insert n (n') m
--                      where


main :: IO ()
main = do
         -- PAKCS: Execution time: 2414404 msec. / elapsed: 5584935 msec 
         -- KICS2: 1.19s 1.60s 37.75s !!!
         -- Curry2Go: ?
         -- print euler14a
         
         -- PAKCS: ????
         -- KICS2: 1.20s 1.78s + ???? Skipped after 5minutes
         -- Curry2Go: ?
         -- print euler14b


         -- PAKCS:
         -- KICS2: KiCS2 compilation time: 1.24s / elapsed: 0:01.47 GHC compilation time: 1.96s / elapsed: 0:02.38 Execution time: 39.34s / elapsed: 0:39.52
         -- Curry2Go: ?
         -- print euler14c


         -- PAKCS: - 
         -- KICS2: >60min
         -- Curry2Go: - 
         -- print euler14d


         -- PAKCS: Execution time: 1159570 msec. / elapsed: 2282313 msec.
         -- KICS2: KiCS2 compilation time: 1.98s / elapsed: 0:02.43 GHC compilation time: 1.20s / elapsed: 0:01.62 Execution time: 18.92s / elapsed: 0:19.00
         -- Curry2Go: - 
         print euler14e


         