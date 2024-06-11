{-
  Euler #3 in Curry.

  Problem 3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List
import HakankUtils

euler3a :: Int
euler3a = last (primeFactors 600851475143)

-- > main1
-- [71,839,1471,6857]
-- PAKCS: Execution time: 427 msec. / elapsed: 492 msec. 
-- KICS2: 1.55s  1.31s 0.01s
-- Curry2Go: Compilation time: 2.17s / elapsed: 0:01.63 Execution time: 1.66s / elapsed: 0:00.99
main1 :: [Int]
main1 = primeFactors 600851475143

-- > main
-- 6857
-- Execution time: 391 msec. / elapsed: 455 msec.
main :: IO ()
main = do
         -- PAKCS: Execution time: 413 msec. / elapsed: 478 msec. 
         -- KICS2: 1.65s 1.24s 0.02s
         -- Curry2Go: Compilation time: 1.85s / elapsed: 0:01.37 Execution time: 1.69s / elapsed: 0:01.03
         print euler3a

