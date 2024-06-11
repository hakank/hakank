{- 
  
  Euler #6 in Haskell

  Problem 6
  """
  The sum of the squares of the first ten natural numbers is,
  1^(2) + 2^(2) + ... + 10^(2) = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

  Hence the difference between the sum of the squares of the first ten 
  natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one 
  hundred natural numbers and the square of the sum.
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import HakankUtils

euler6a =  sum [1..100]^2 - sum [i^2 | i <- [1..100]]

euler6b =  sum t^2 - sum [i^2 | i <- t]
           where t = [1..100]

euler6c = (sum [1..100])^2 - (sum $ map (^2) [1..100])

main = do
         -- print euler6a -- (0.01 secs, 158,144 bytes)
         print euler6b -- (0.00 secs, 151,040 bytes)
         -- print euler6c -- (0.00 secs, 154,192 bytes)
