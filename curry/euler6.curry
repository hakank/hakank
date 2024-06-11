{- 
  
  Euler #6 in Curry

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
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils

euler6a :: Int
euler6a =  sum [1..100]^2 - sum [i^2 | i <- [1..100]]

euler6b :: Int
euler6b =  sum t^2 - sum [i^2 | i <- t]
           where t = [1..100]

euler6c :: Int
euler6c = (sum [1..100])^2 - (sum $ map (^2) [1..100])

main :: IO ()
main = do
         -- PAKCS: Execution time: 11 msec. / elapsed: 20 msec. 
         -- KICS2: 1.19s+1.42s+0.00s
         -- Curry2Go: Compilation time: 2.43s / elapsed: 0:01.77 Execution time: 0.07s / elapsed: 0:00.05
         -- print euler6a
         
         -- PAKCS: Execution time: 5 msec. / elapsed: 11 msec. 
         -- KICS2: 1.26s+1.44s+0.00s
         -- Curry2Go: Compilation time: 2.07s / elapsed: 0:01.59 Execution time: 0.07s / elapsed: 0:00.05
         print euler6b
         
         -- PAKCS: Execution time: 16 msec. / elapsed: 20 msec. 
         -- KICS2: 1.25s+1.39s+0.00s
         -- Curry2Go: Compilation time: 2.19s / elapsed: 0:01.60 Execution time: 0.09s / elapsed: 0:00.05
         -- print euler6c