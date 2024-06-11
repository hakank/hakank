{- 
  
  Euler #7 in Haskell

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils


euler7a = last $ take 10001 $ iterate nextPrime 2

-- A little cheating
euler7b = last $ take 10001 $ primesN 200000

-- simple loop version
euler7c' n a = if a == 10001 then
                  n
               else
                  euler7c' (nextPrime n) (a+1)

euler7c = euler7c' 2 1

-- Using the quite slower isPrime2
euler7d = last $ take 10001 $ primesN2 200000

main = do
         -- print euler7a -- (0.66 secs, 601,912,264 bytes)
         -- print euler7b -- (0.60 secs, 565,373,416 bytes)
         print euler7c -- (0.66 secs, 603,897,368 bytes)
         -- print euler7d -- (0.87 secs, 665,496,072 bytes)