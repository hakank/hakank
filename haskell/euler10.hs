{- 
  
  Euler #10 in Haskell

  Problem 10
  """ 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
  Find the sum of all the primes below two million.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

-- (32.41 secs, 32,058,821,632 bytes)
euler10a = sum $ primesN 2000000

-- (28.59 secs, 20,718,060,864 bytes)
euler10b = sum (takeWhile (< 2000000) primes)

-- 
euler10c' 0 a = a
euler10c' n a | n > 0 = if isPrime n then
                          euler10c' (n-1) (a+n)
                        else
                          euler10c' (n-1) a
                                    
-- (33.76 secs, 32,881,321,776 bytes)
euler10c = euler10c' 2000000 0

main = do
         -- print euler10a -- 32.41s
         print euler10b -- 28.59s
         -- print euler10c -- 33.76s