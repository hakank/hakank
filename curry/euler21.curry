{- 
  
  Euler #21 in Curry

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
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

euler21a :: Int
euler21a = sum $ nub $ concat [[n,d] | n <- [1..10000], d <- [sumDivisorsSlow n], d /= n, n == sumDivisorsSlow d]

euler21b :: Int
euler21b = sum $ nub $ concat [[n,d] | n <- [1..10000], d <- [sumDivisors n], d /= n, d < n, n == sumDivisors d]

main :: IO ()
main = do
         -- PAKCS: Execution time: 288785 msec. / elapsed: 321368 msec. 
         -- KICS2: 1.06s 1.16s 4.73s
         -- Curry2Go: Compilation time: 2.13s / elapsed: 0:01.61 Execution time: 1326.25s / elapsed: 9:14.83
         -- print euler21a
         
         -- PAKCS: Execution time: 38097 msec. / elapsed: 44977 msec. 
         -- KICS2: 1.68s 1.96s 1.02s
         -- Curry2Go:Compilation time: 2.21s / elapsed: 0:01.81 Execution time: 166.83s / elapsed: 1:23.50
         print euler21b