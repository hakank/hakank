{- 
  
  Euler #21 in Haskell

  Problem 21
  """
  Let d(n) be defined as the sum of proper divisors of n (numbers less 
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
  pair and each of a and b are called amicable numbers.
  
  For example, the proper divisors of 220 are 
  1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
  The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  
  Evaluate the sum of all the amicable numbers under 10000.
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils


euler21a = sum $ nub $ concat [[n,d] | n <- [1..10000], d <- [sumDivisorsSlow n], d /= n, n == sumDivisorsSlow d]

euler21b = sum $ nub $ concat [[n,d] | n <- [1..10000], d <- [sumDivisors n], d /= n, d < n, n == sumDivisors d]

main = do
         -- print euler21a -- (11.25 secs, 8,583,968,056 bytes)
         print euler21b -- (0.92 secs, 974,314,496 bytes)