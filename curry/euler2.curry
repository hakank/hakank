{-

  Euler #2 in Curry.

  Problem 2
  """
  Each new term in the Fibonacci sequence is generated by adding the 
  previous two terms. By starting with 1 and 2, the first 10 terms will be:

  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  Find the sum of all the even-valued terms in the sequence which do not 
  exceed four million.
  """

  Note: Curry2Go give wrong answer for euler2b and it's probably because it cannot
  handle large integers.
   > [f | n <- [1..100], f <- [fib(n)], even f && f < 4000000]
   Here's the result from PAKCS and KiCS2:
     [2,8,34,144,610,2584,10946,46368,196418,832040,3524578]

   But curry2go has some strange result. It seems to be some kind of overflow...
   [2, 8, 34, 144, 610, 2584, 10946, 46368, 196418, 832040, 3524578, -6246583658587674878, -3659523366270331776, -2437933049959450366]


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List
import HakankUtils

euler2a :: Int
euler2a = sum . filter even . takeWhile (<4000000) $ fiblist 1 2

euler2b :: Int
euler2b = sum [f | n <- [1..100], let f = fib(n), even f, f < 4000000]

-- From https://wiki.haskell.org/Euler_problems/1_to_10
-- Neat with the zipWith
euler2c :: Int
euler2c = sum [ x | x <- takeWhile (<= 4000000) fibs, even x]
           where
             fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- The time is the fastest of two runs
main :: IO ()
main = do
         -- PACKCS: Execution time: 2 msec. / elapsed: 3 msec. 
         -- KICS2: KICS2: 2.01s 1.63s 0.00s
         -- Curry2Go: Compilation time: 1.96s / elapsed: 0:01.48 Execution time: 0.08s / elapsed: 0:00.02
         -- print euler2a
         
         -- PACKCS: Execution time: 47 msec. / elapsed: 50 msec 
         -- KICS2: 1.98s 1.57s 0.01s
         -- Curry2Go: Compilation time: 2.14s / elapsed: 0:01.66 Execution time: 0.12s / elapsed: 0:00.11  Wrong result: 6102703998896708328 !!
         -- print euler2b
         
         -- PACKCS: Execution time: 1 msec. / elapsed: 1 msec. 
         -- KICS2: 1.72s 1.25s 0.00s
         -- Curry2Go: Compilation time: 2.20s / elapsed: 0:01.71 Execution time: 0.07s / elapsed: 0:00.02
         print euler2c
         
         
