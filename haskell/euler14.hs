{- 
  
  Euler #14 in Haskell

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

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import HakankUtils
import Data.Function (fix)


memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

-- collatz n | n `mod` 2 == 0 = n `div` 2
-- collatz n | n `mod` 2 == 1 = (3*n) + 1
collatz :: Integral a => a -> a
collatz n = if n `mod` 2 == 0 then
              n `div` 2
            else
              (3*n) + 1
                  
collatzSeq :: Integral a => a -> [a]
collatzSeq n = takeWhile (>1) $ iterate collatz n

maxCollatz m = maximum $ map (\n -> (length $ collatzSeq n,n) ) [2..m]

--
--
-- Based on
-- https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html
-- Same as collatz but with an extra first argument f
-- Much slower
collatz2 f n = if n `mod` 2 == 0 then
              n `div` 2
            else
              (3*n) + 1
                    
collatzMemo = fix (memoize . collatz2)
               
-- collatzSeq :: Integral a => a -> [a]
collatzSeqMemo n = takeWhile (>1) $ iterate collatzMemo n

maxCollatzMemo m = maximum $ map (\n -> (length $ collatzSeqMemo n,n) ) [2..m]                   

-- Brute force: 152.71s
euler14a :: (Int, Integer)
-- euler14a = maximum $ map (\n -> (length $ collatzSeq n,n) ) [2..1000000]
euler14a = maxCollatz 1000000

-- Memoized version inspired by
-- https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html
-- This is _much_ slower...
-- euler14b = maximum $ map (\n -> (length $ collatzSeqMemo n,n) ) [2..10000]
euler14b = maxCollatzMemo 1000000 -- Out of memory


-- Using explicit recursion with accumulator
collatzLen3 n aux
  | n == 1 = aux
  | otherwise = if n `mod` 2 == 0 then
                          collatzLen3 (n `div` 2) (aux+1)
                      else
                          collatzLen3 ((n*3)+1) (aux+1)

euler14c = maximum [(collatzLen3 n 1, n) | n <- [1..1000000], let len = collatzLen3 n 0 ]

--
-- As euler14a but only odd integers (since odd numbers are the one that grow).
--
euler14e :: (Int,Int)
euler14e = maximum $ map (\n -> (length $ collatzSeq n,n) ) [3,5..1000000]

                                                                        
--          
main :: IO () 
main = do
         -- print euler14a -- (144.00 secs, 103,569,255,280 bytes)

         -- print euler14b -- outof memory

         -- print euler14c -- (125.92 secs, 108,737,206,496 bytes)

         print euler14e -- (69.02 secs, 57,893,858,304 bytes)