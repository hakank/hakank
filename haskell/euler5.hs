{- 
  
  Euler #5 in Haskell

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers 
  from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers 
  from 1 to 20?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

euler5a = foldr1 lcm [2..20] -- foldr, a little faster
euler5b = foldl1 lcm [2..20] -- foldl

euler5c = last $ scanl1 lcm [2..20]
euler5d = last $ take 19 $ scanl1 lcm [2..]

main = do
         print euler5a -- (0.01 secs, 90,560 bytes)
         -- print euler5b -- (0.00 secs, 89,896 bytes)
         -- print euler5c -- (0.00 secs, 91,184 bytes)
         -- print euler5d -- (0.00 secs, 92,280 bytes)

